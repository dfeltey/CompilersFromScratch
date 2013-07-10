module Tokens where

import Lex
import RegEx
import Data.Char
import Prelude hiding (seq)
import CEK

lexS :: String -> Either String [Token]
lexS s = lexer rules s

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
			| IFT
			| IntT Integer
			| BoolT Bool
			| VarT Name
			| PrintT
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
		 , (word "if", const IFT)
		 , (word "print", const PrintT)
		 , (seq (Sym isDigit) (star $ Sym isDigit), IntT . read)
		 , (word "True" `alt` word "False", BoolT . read)
		 , (Sym isAlpha `seq` (star $ Sym isAlpha) ,VarT)
		 ]











