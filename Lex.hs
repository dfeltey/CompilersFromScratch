module Lex where

import RegEx
type Regex = RegEx Char



tokenize :: Int -> Int -> [(Regex,String->t)] -> String -> Either String t
tokenize line char rules string = go line char rules string [] where
                                  go = undefined

{-
lex :: [(RegW c Bool,[c]->t)] -> [c] -> [Token t]
lex rules str = fst $ go rules str [] where
                go [] _ s = ([],s)
                go _ [] _ = ([],[])
                go rules (c:cs) s = let step = mapFst (derivativeW c) rules in
                                    case or $ map fst $ mapFst emptyW step of
                                        False -> let (x,y) = go rules (c:cs) [] in ((Tok $ (snd . head) rules $ reverse s) : x, y)
                                        True  -> undefined

tokenize :: [(RegW c Bool,[c]->t)] -> [c] -> Maybe (Token t,[c])
tokenize [] _ = Nothing -- maybe this should be an error??
tokenize _ [] = Nothing
tokenize rules str = go rules str [] where
                     go rules (c:cs) s = let step = mapFst (derivativeW c) rules in 
                         case or $ map fst $ mapFst emptyW step of
                            False -> let 
                                accepts = filter (\(x,y) -> emptyW x) rules 
                                rule = head accepts
                                in Just $ (Tok $ snd rule $ reverse s,(c:cs))
                            -- This above case should work, but I can't see immediately that
                            -- there is always an accepting rule in the current list
                            -- should probably also check that and return error otherwise...
                            True -> undefined



mapFst :: (a->t) -> [(a,b)] -> [(t,b)]
mapFst f ps = map (\(x,y) ->(f x, y)) ps 
-}