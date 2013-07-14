module SExpr where

import CEK

data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          | PrintS SExpr
          deriving(Show)

desugar :: SExpr -> Expr
desugar sexp = case sexp of 
    VarS x -> Var x
    ValS n -> Val n
    BoolS b -> BoolE b
    AppS s1 s2 -> App (desugar s1) (desugar s2)
    LambdaS x s -> Lambda x (desugar s)
    BinopS op s1 s2 -> case op of
    	   OR -> App (Lambda "$z" (IF (Var "$z") (Var "$z") (desugar s2))) (desugar s1)
	   AND -> App (Lambda "$z" (IF (Var "$z") (desugar s2) (Var "$z") )) (desugar s1)
	   _ -> Binop op (desugar s1) (desugar s2)
    IFS b s1 s2 -> IF (desugar b) (desugar s1) (desugar s2)
    LetS (x,s1) s2 -> App (Lambda x (desugar s2)) (desugar s1)
    PrintS e -> PrintE (desugar e)


fact = LetS ("fact"
            ,LambdaS "n" 
              (IFS (BinopS Eq (VarS "n") (ValS 1)) 
                (ValS 0) 
                (BinopS Mul (VarS "n") (AppS (VarS "fact") (BinopS Sub (VarS "n") (ValS 1))))))
            (AppS (VarS "fact") (ValS 5))