module Parser where

import Tokens
import Parse
import Lex
import SExpr
import CEK

wrap :: Parser Token a ->  Parser Token a
wrap p = pSym LParenT *> p <* pSym RParenT

parseExpr :: Parser Token SExpr
parseExpr =  parseApp
		 <|> parseLambda
		 <|> parseBinop
		 <|> parseVar
		 <|> parseBool
		 <|> parseInt
		 <|> parseIF
		 <|> parseLet

parseApp :: Parser Token SExpr
parseApp = wrap $ AppS <$> parseExpr <*> parseExpr

parseLambda :: Parser Token SExpr
parseLambda = wrap $ (\(VarS x) -> LambdaS x) <$> (pSym LambdaT *> parseVar) <*> parseExpr 

parseBinop :: Parser Token SExpr
parseBinop = wrap $ (\opT -> BinopS (fromT opT)) <$> parseOp <*> parseExpr <*> parseExpr

parseIF :: Parser Token SExpr
parseIF = wrap $ IFS <$> parseExpr <*> parseExpr <*> parseExpr

parseLet :: Parser Token SExpr
parseLet = wrap $ (\(VarS x,s) -> LetS (x,s)) <$> (wrap $ (,) <$> parseVar <*> parseExpr) <*> parseExpr 


parseVar :: Parser Token SExpr
parseVar = (\(VarT x) -> VarS x) <$> pSatisfy isVar where
	isVar t = case t of
		VarT _ -> True
		otherwise -> False


parseInt :: Parser Token SExpr
parseInt = (\(IntT n) -> ValS n) <$> pSatisfy isIntTok where
	isIntTok t = case t of
		IntT _ -> True
		otherwise -> False

parseBool :: Parser Token SExpr
parseBool = (\(BoolT b) -> BoolS b) <$> pSatisfy isBoolTok where
	isBoolTok t = case t of
		BoolT _ -> True
		otherwise -> False


parseOp :: Parser Token Token
parseOp = pChoice $ map pSym [ AddT
							 , MulT
							 , DivT
							 , SubT
							 , ModT
							 , EqT
							 , LtT
							 , GtT
							 , LeqT
							 , GeqT
							 , ANDT
							 , ORT]

fromT :: Token -> Op
fromT t = case t of
	AddT -> Add
	MulT -> Mul
	DivT -> Div
	SubT -> Sub
	ModT -> Mod
	EqT -> Eq
	LtT -> Lt
	GtT -> Gt
	LeqT -> Leq
	GeqT -> Geq
	ANDT -> AND
	ORT  -> OR
	_ -> error "not an operator"












