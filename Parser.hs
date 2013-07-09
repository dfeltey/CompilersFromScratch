module Parser where

import Tokens
import Parse
import Lex
import SExpr
import CEK


{-
data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          deriving(Show


data Token  = LParenT
			| RParenT
			| SpaceT
			| LambdaT
			| AddT
			| MulT
			| DivT
			| SubT
			| ModT
			| EqT
			| LtT
			| GtT
			| LeqT
			| GeqT
			| LetT
			| IntT Integer
			| BoolT Bool
			| VarT Name
			deriving(Show)

-}
wrap :: Parser Token a ->  Parser Token a
wrap p = pSym LParenT *> p <* pSym RParenT

parseExpr :: Parser Token SExpr
parseExpr =  parseApp
		 <|> parseLambda
		 <|> parseBinop
		 <|> (VarS <$> parseVar)
		 <|> parseBool
		 <|> parseInt
		 <|> parseIF
		 <|> parseLet

parseApp :: Parser Token SExpr
parseApp = wrap $ AppS <$> parseExpr <*> parseExpr

parseLambda :: Parser Token SExpr
parseLambda = wrap $ LambdaS <$> parseVar <*> parseExpr 

parseBinop :: Parser Token SExpr
parseBinop = wrap $ BinopS <$> parseOp <*> parseExpr <*> parseExpr

parseIF :: Parser Token SExpr
parseIF = wrap $ IFS <$> parseExpr <*> parseExpr <*> parseExpr

parseLet :: Parser Token SExpr
parseLet = wrap $ LetS <$> (wrap $ (,) <$> parseVar <*> parseExpr) <*> parseExpr 


parseVar :: Parser Token Name
parseVar = P $ \inp -> case inp of
				(VarT x):_ -> [(x,[])]
				otherwise -> []

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


parseOp :: Parser Token Op
parseOp = undefined



















