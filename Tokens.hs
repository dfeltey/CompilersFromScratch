module Tokens where

import Lex
import RegEx
import Data.Char
import Prelude hiding (seq)
import CEK

{-data SExpr = AppS SExpr SExpr
          | LambdaS Name SExpr
          | BinopS Op SExpr SExpr
          | VarS Name
          | ValS Integer
          | BoolS Bool
          | IFS SExpr SExpr SExpr
          | LetS (Name,SExpr) SExpr 
          deriving(Show
-}
notSpace :: Token -> Bool
notSpace SpaceT = False
notSpace _ = True

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
			| ORT
			| ANDT
			| IntT Integer
			| BoolT Bool
			| VarT Name
			deriving(Show,Eq)

rules = [  (Sym $ isSpace, const SpaceT)
		 , (symC '(', const LParenT)
		 , (symC ')', const RParenT)
		 , (word "lambda", const LambdaT)
		 , (symC '+', const AddT)
		 , (symC '*', const MulT)
		 , (symC '/', const DivT)
		 , (symC '-', const SubT)
		 , (symC '%', const ModT)
		 , (symC '=', const EqT)
		 , (symC '<', const LtT)
		 , (symC '>', const GtT)
		 , (word "<=", const LeqT)
		 , (word "=", const GeqT)
		 , (word "or", const ORT)
		 , (word "and", const ANDT)
		 , (word "let", const LetT)
		 , (seq (Sym isDigit) (star $ Sym isDigit), IntT . read)
		 , (word "True" `alt` word "False", BoolT . read)
		 , (Sym isAlpha `seq` (star $ Sym isAlpha) ,VarT)
		 ]











