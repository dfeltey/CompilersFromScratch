data Expr = App Expr Expr
          | Lambda Name Expr
          | Binop Op Expr Expr
          | Var Name
          | Val Integer
          | BoolE Bool
          deriving(Show)

data Val = VNum Integer
         | VVar Name
         | VBool Bool
         deriving(Show)

applyOp :: Op -> Val -> Val -> Val
applyOp op (VNum n) (VNum m) = case op of
                                Add -> VNum (n+m)
                                Sub -> VNum (n-m)
                                Mul -> VNum (n*m)
                                Div -> VNum (n `div` m)
                                Eq  -> VBool $ n == m
                                Lt  -> VBool $ n < m
                                Gt  -> VBool $ n > m
                                Leq -> VBool $ n <= m
                                Geq -> VBool $ n >= m
applyOp op (VBool b1) (VBool b2) = case op of
                                Eq -> VBool $ b1 == b2
                                OR -> VBool $ b1 || b2
                                AND -> VBool $ b1 && b2
applyOp _ _ _ = error "attempt to add non-integer values"

data Op = Add
        | Sub
        | Mul
        | Div
        | Eq 
        | Lt
        | Gt
        | Leq
        | Geq 
        | OR
        | AND
        deriving(Show)

type Name = String

data Code = Push [Code]
          | PushI Integer
          | PushB Bool
          | Access Name
          | Close Name [Code]
          | OpC Op
          | Load [Code]
          deriving(Show)


data Clos = Clos (Val,[Code],Env) deriving(Show)

getVal :: Clos -> Val
getVal (Clos (v,c,e)) = v

type Env = [(Name,Clos)]

data Cont = MT
          | FN Clos Cont
          | AR [Code] Env Cont
          | OP Op [Clos] [Code] Env Cont
          deriving(Show)   

compile :: Expr -> [Code]
compile (App e1 e2) = Push (compile e2) : compile e1
compile (Lambda x e) = [Close x (compile e)]
compile (Var x) = [Access x]
compile (Val n) = [PushI n]
compile (BoolE b) = [PushB b]
compile (Binop op e1 e2) = [compileOp op, Load $ compile e1, Load $ compile e2]
-- Push is the wrong instruction to handle binops, push always sets up an arg continuation
-- but that isn't what is needed for a binop, because they are not applied the way
-- that functions are, need a Load instruction to handle these ...

-- compile (Binop op e1 e2) = Push (compile e1) : Push (compile e2) : [compileOp op]
-- compile (Binop op e1 e2) (CompileOp op) (compile e1) (compile e2)
-- Use the op to build the Op continuation then can push and manipulate the arguments
-- doing it the other way forces the arguments into unwanted AR continuations
-- which are inappropriate in this situation 


compileOp :: Op -> Code
compileOp = OpC

type Output = [String]
type MachineState = ([Code],Env,Cont)
type ContState = (Cont,Clos)
-- type Program = MachineState

load :: [Code] -> MachineState
load p = (p,[],MT)

isLoad :: Code -> Bool
isLoad (Load _) = True
isLoad _ = False


eval3 :: MachineState -> Clos
eval3 (Access x:c,e,k) = eval2 (k,envLookup e x (error $ "couldn't find " ++ x ++" in environment"))
eval3 (PushI n:c,e,k) = eval2 (k,Clos (VNum n,[],[]))
eval3 (PushB b:c,e,k) = eval2 (k,Clos(VBool b,[],[]))
eval3 (Close x c':c,e,k) = eval2 (k,Clos (VVar x,c',e))
eval3 (Push c':c,e,k) = eval3 (c,e,AR c' e k)
-- eval3 (Load c : c',e,k) = eval3 (c++c',e,k)
eval3 (OpC op:Load c:c',e,k) = eval3 (c,e,OP op [] c' e k) -- this is probably wrong, but there is no way to immediately create a closure to pass to this continuation
eval3 _ = error "eval3"

eval2 :: ContState -> Clos
eval2 (AR c e k,v) = eval3 (c,e,FN v k)
eval2 (FN (Clos (VVar x,c,e)) k,v) = eval3 (c,update e x v,k)
eval2 (OP op vs (Load c:c') e k, v) = eval3 (c,e,OP op (v:vs) c' e k) 
eval2 (OP op vs c e k,v) = eval2 (k,Clos (applyOp op (getVal (head vs)) (getVal v),[],[]))
eval2 (MT,v) = v
eval2 _ = error "eval2" 


envLookup :: (Eq a) => [(a,b)] -> a -> b -> b
envLookup [] a b = b
envLookup ((k,v):l) a b 
    | a == k = v
    | otherwise = envLookup l a b
update :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
update [] a b = [(a,b)]
update ((k,v):l) a b
    | a == k = (k,b):l
    | otherwise = (k,v) : update l a b  



type Program = [Code]
run :: Program -> Clos
run = eval3 . load



















