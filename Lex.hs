module Lex where

import Prelude hiding (seq)
import RegEx
import Control.Monad.Error
import Data.Char
type Regex = RegEx Char

mapFst :: (a->t) -> [(a,b)] -> [(t,b)]
mapFst f ps = map (\(x,y) ->(f x, y)) ps 


lexer :: [(Regex,String -> t)] -> String -> Either String [t]
lexer rules s = lexAt 0 0 rules s where
              lexAt line char rules s = do
                      (t,s',line',char') <- tokenize line char rules s
                      case s' of
                        [] -> return [t]
                        str -> do
                            rest <- lexAt line' char' rules str
                            return $ t:rest

tok :: [(Regex,String->t)] -> String -> Either String (t,String,Int,Int)
tok = tokenize 0 0 


tokenize :: Int -> Int -> [(Regex,String->t)] -> String -> Either String (t,String,Int,Int)
tokenize line char rules string = go line char rules string [] where
                                  go line char [] _ _ = Left "Error: Empty rule list."
                                  go line char rules [] s = let rules' = filter ((\(a,b) -> empty a)) rules in
                                                                        case rules' of
                                                                            [] -> Left $ "Line: " ++ show line ++ " Char: " ++ show char ++ "\nError: Empty string unexpected"
                                                                            (r:rs) -> Right (snd r $ reverse s,[],line,char)
                                  go line char rules (c:cs) s = let char' = if c == '\n' then 0 else (char + 1)
                                                                    line' = line + if c == '\n' then 1 else 0
                                                                    step = mapFst (derivative c) rules in
                                                                        case filter (\(a,b) -> notNull a) step of
                                                                            (r:rs) -> go line' char' step cs (c:s)
                                                                            [] -> case filter (\(a,b) -> empty a) rules of
                                                                                [] -> Left $ "Line: " ++ show line ++ " Char: " ++ show char ++ "\nError: No matching regular expressions."
                                                                                (r:rs) -> Right $ (snd r $ reverse s,(c:cs),line,char)






