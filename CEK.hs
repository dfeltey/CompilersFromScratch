module CEK where


nested = PrintE (App (App (App (Lambda "x" (Lambda "y" (Lambda "x" (Binop Add (Var "y") (Var "x"))))) (BoolE True)) (Val 5)) (Val 3))

data Expr = App Expr Expr
          | Lambda Name Expr
          | Binop Op Expr Expr
          | Var Name
          | Val Integer
          | BoolE Bool
          | IF Expr Expr Expr
          | PrintE Expr 
          deriving(Show)

data Val = VNum Integer
         | VVar Name
         | VBool Bool
         deriving(Show)

truthy :: Clos -> Bool
truthy (Clos (VBool b,_,_)) = b
truthy (Clos (VNum n,_,_)) = n /= 0
truthy _ = False

applyOp :: Op -> Val -> Val -> Val
applyOp op (VNum n) (VNum m) = case op of
                                Add -> VNum (n+m)
                                Sub -> VNum (n-m)
                                Mul -> VNum (n*m)
                                Div -> VNum (n `div` m)
                                Mod -> VNum (n `mod` m)
                                Eq  -> VBool $ n == m
                                Lt  -> VBool $ n < m
                                Gt  -> VBool $ n > m
                                Leq -> VBool $ n <= m
                                Geq -> VBool $ n >= m
applyOp op (VBool b1) (VBool b2) = case op of
                                Eq -> VBool $ b1 == b2
                                OR -> VBool $ b1 || b2
                                AND -> VBool $ b1 && b2
                                otherwise -> error $ "Error: Operation: " ++ show op ++ " is not supported on boolean arguments."
applyOp op v1 v2 = error $ "Error: Operation: " ++ show op ++ " is not supported on these arguments."

data Op = Add
        | Sub
        | Mul
        | Div
        | Mod
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
          | Branch [Code] [Code] [Code]
          | Print 
          deriving(Show)




data Cont = MT
          | FN Clos Cont
          | AR [Code] Env Cont
          | OP Op [Clos] [Code] Env Cont
          | IFC [Code] [Code] Env Cont
          | PrintC  Cont
          deriving(Show)   

compile :: Expr -> [Code]
compile (App e1 e2) = Push (compile e2) : compile e1
compile (Lambda x e) = [Close x (compile e)]
compile (Var x) = [Access x]
compile (Val n) = [PushI n]
compile (BoolE b) = [PushB b]
compile (Binop op e1 e2) = [OpC op, Load $ compile e1, Load $ compile e2]
compile (IF b e1 e2) = [Branch (compile b) (compile e1) (compile e2)] 
compile (PrintE e) = Print : compile e



type MachineState = ([Code],Env,Cont,[String])
type ContState = (Cont,Clos,[String])

data Clos = Clos (Val,[Code],Env) deriving(Show)

getVal :: Clos -> Val
getVal (Clos (v,c,e)) = v

type Env = [(Name,Clos)]

load :: [Code] -> MachineState
load p = (p,[],MT,[])

isLoad :: Code -> Bool
isLoad (Load _) = True
isLoad _ = False


eval3 :: MachineState -> (Clos,[String])
eval3 (Access x:c,e,k,o) = eval2 (k,envLookup e x (error $ "couldn't find " ++ x ++" in environment"),o)
eval3 (PushI n:c,e,k,o) = eval2 (k,Clos (VNum n,[],[]),o)
eval3 (PushB b:c,e,k,o) = eval2 (k,Clos(VBool b,[],[]),o)
eval3 (Close x c':c,e,k,o) = eval2 (k,Clos (VVar x,c',e),o)
eval3 (Push c':c,e,k,o) = eval3 (c,e,AR c' e k,o)
eval3 (OpC op:Load c:c',e,k,o) = eval3 (c,e,OP op [] c' e k,o) 
eval3 (Branch b c1 c2 :c,e,k,o) = eval3 (b,e,IFC c1 c2 e k,o)
eval3 (Print:c,e,k,o) = eval3 (c,e,PrintC k,o)
eval3 _ = error "eval3"

eval2 :: ContState -> (Clos,[String])
eval2 (AR c e k,v,o) = eval3 (c,e,FN v k,o)
eval2 (FN (Clos (VVar x,c,e)) k,v,o) = eval3 (c,update e x v,k,o)
eval2 (OP op vs (Load c:c') e k, v,o) = eval3 (c,e,OP op (v:vs) c' e k,o) 
eval2 (OP op vs c e k,v,o) = eval2 (k,Clos (applyOp op (getVal (head vs)) (getVal v),[],[]),o)
eval2 (MT,v,o) = (v,o)
eval2 (IFC c1 c2 e k,v,o) = eval3 (if truthy v then c1 else c2,e,k,o)
eval2 (PrintC k,v,o) = eval2 (k,v, printClos v o) 
eval2 _ = error "eval2" 

printClos :: Clos -> [String] -> [String]
printClos (Clos (v,_,_)) o = case v of
    VNum n -> o ++ [show n]
    VBool b -> o ++ [show b]
    VVar _ -> o ++ ["**Closure**"]


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
run :: Program -> (Clos,[String])
run = eval3 . load



















