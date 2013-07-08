import CEK

data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          deriving(Show)

desugar :: SExpr -> Expr
desugar sexp = case sexp of 
    VarS x -> Var x
    ValS n -> Val n
    BoolS b -> BoolE b
    AppS s1 s2 -> App (desugar s1) (desugar s2)
    LambdaS x s -> Lambda x (desugar s)
    BinopS op s1 s2 -> Binop op (desugar s1) (desugar s2)
    IFS b s1 s2 -> IF (desugar b) (desugar s1) (desugar s2)
    LetS (x,s1) s2 -> App (Lambda x (desugar s2)) (desugar s1)