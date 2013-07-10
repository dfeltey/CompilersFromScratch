% Compilers From Scratch
% Daniel Feltey
% July 10, 2013


# The Compilation Process

A typical workflow 

Source Code -> Lexer -> Parser -> Code Generator -> Machine Code

# Outline

- Regular Expressions
- Lexing
- Parsing
- Desugaring
- The CEK machine
- Code Generation


# Regular Expressions

3 Basic operations 

- Alternation
- Concatenation or sequencing
- Repetition or Kleene Star

Additionally

- Symbols
- Epsilon: The empty string
- Null: The empty language

# Regular Expressions in Haskell

~~~haskell
data RegEx c = Null
			 | Eps
			 | Sym c
			 | Alt (RegEx c) (RegEx c)
			 | Seq (RegEx c) (RegEx c)
			 | Star (RegEx c)
~~~

> - This is fine, but how do we represent a regex that matches any number?

> -  ~~~haskell
Alt (Sym '1') (Alt (Sym '2') (Alt (Sym '3') (Alt (Sym '4') (Alt (Sym '5') ...))))
~~~

# A Slight Modification

~~~haskell
data RegEx c = Null
			 | Eps
			 | Sym (c -> Bool)
			 | Alt (RegEx c) (RegEx c)
			 | Seq (RegEx c) (RegEx c)
			 | Star (RegEx c)
~~~

> - We add boolean "weights" to the symbols.

> - Now to represent a regex for any digit character

> - ~~~haskell
import Data.Char
Sym (isDigit)
~~~

# Matching Strings

Two approaches 

- Destructure a string like a regex 

- Destructure a regex like a string 

# RegEx Derivatives

Decomposing a regex

- Match one character at a time like a list or Haskell String
- Take derivatives

~~~
			 D_c	
Null       -- c -->  Null
Eps        -- c -->  Null
Sym p      -- c -->  if p c then Eps else Null
Alt r1 r2  -- c -->  Alt (D_c r1) (D_c r2)
Seq r1 r2  -- c -->  Alt (Seq (D_c r1) r2) (Seq (empty r1) (D_c r2))
Star r     -- c -->  Eps
~~~ 

# Empty

What is empty?

~~~
empty Null = Null
empty Eps = Eps
empty (Sym _) = Null
empty (Alt r1 r2) = Alt (empty r1) (empty r2)
empty (Seq r1 r2) = Seq (empty r1) (empty r2)
empty (Star _) = Eps
~~~

- Not implemented like this, but close enough

# Matching

~~~haskell
match :: RegEx Char -> String -> Bool
match r [] = empty r
match r (c:cs) = match (derivative c r) cs
~~~

- Lex with a list of (regex,String->token) pairs
- Take as many derivatives as possible then apply the function

# An Enriched Lambda Calculus

The Language we are implementing

~~~
<SExpr> : ( <SExpr> <SExpr> )
	    | (lambda <Var> <SExpr> )
	    | (<Binop> <SExpr> <SExpr> )
	    | (if <SExpr> <SExpr> <SExpr> )
	    | (let ( <Var> <SExpr> ) <SExpr> )
	    | (print <SExpr> )
	    | <Var>
	    | <Int>
	    | <Bool>

<Var>   : [a-zA-Z]+
<Int>   : [0-9]+
<Bool>  : True | False
<Binop> : + | - | * | / | = | < | > | <= | >=
	    | % | and | or
~~~

# Abstract Syntax in Haskell

~~~haskell
data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          | PrintS SExpr
~~~

# Challenge

- Create a token data type for this language
- Specify a lexer

Exercises: https://www.fpcomplete.com/user/dfeltey/compilers-from-scratch

# Desugaring 

Recall:

~~~haskell
data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          | PrintS SExpr
~~~

- Is this more than we need?

# A Smaller Language

Consider:

~~~haskell
data Expr = App Expr Expr
          | Lambda Name Expr
          | Binop Op Expr Expr
          | Var Name
          | Val Integer
          | BoolE Bool
          | IF Expr Expr Expr
          | PrintE Expr 
          deriving(Show)
~~~

- Is this language as expressive as the old one?
- Write a function to convert between SExprs and Exprs.


# The CEK Machine

- A mechanical model of the lambda calculus
- A "register" machine with 3 registers
- (C)ontrol (E)nvironment (K)ontinuation


# CEK explained

We will be writing byte code for a CEK machine

- The Control register holds a stack of bytecode to be executed
- The Environment register associates names with their values
- The Continuation register tells us what to do with a value 


# Our Language again

What are the core concepts in our language?

~~~haskell
data Expr = App Expr Expr
          | Lambda Name Expr
          | Binop Op Expr Expr
          | Var Name
          | Val Integer
          | BoolE Bool
          | IF Expr Expr Expr
          | PrintE Expr 
          deriving(Show)
~~~

> - Function application
> - Lambda abstraction
> - Binary operators
> - Variable access
> - Constants
> - If expressions
> - Printing 

# From Concepts to ("Byte") Code

Designing an instruction set for our core language

- The easy cases

~~~haskell
Val 53 ---> PushI 53 
~~~

~~~haskell
BoolE True ---> PushB True
~~~

~~~haskell
Var "x" ---> Access "x"
~~~

# From Concepts to ("Byte") Code

- The middle cases

~~~haskell
Lambda "x" e ---> Close "x" [compile e]
~~~

~~~haskell
IF e1 e2 e3 ---> Branch [compile e1] [compile e2] [compile e3]
~~~

~~~haskell
PrintE e ---> Print : compile e 
~~~

# From Concepts to ("Byte") Code

- The hard cases

~~~haskell
App e1 e2 ---> Push (compile e2) : compile e1
~~~

~~~haskell
Binop op e1 e2 ---> [OpC op, Load (compile e1), Load (compile e2)]
~~~

# Recap

Our "Byte" code as a Haskell data type

~~~haskell
data Code = Push [Code]
          | PushI Integer
          | PushB Bool
          | Access Name
          | Close Name [Code]
          | OpC Op
          | Load [Code]
          | Branch [Code] [Code] [Code]
          | Print 
~~~


# Continuations

Our CEK machine will build continuations for all "complex" expressions

- The (K)ontinuation register is a stack of continuations
- It keeps track of the next step of evaluation

> - ~~~haskell
data Cont = MT
          | FN Clos Cont
          | AR [Code] Env Cont
          | OP Op [Clos] [Code] Env Cont
          | IFC [Code] [Code] Env Cont
          | PrintC  Cont
          deriving(Show)
~~~











