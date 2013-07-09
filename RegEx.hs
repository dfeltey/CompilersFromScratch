module RegEx where

import Prelude hiding (seq)

data RegEx c = Null
             | Eps
             | Sym (c -> Bool)
             | Alt (RegEx c) (RegEx c)
             | Seq (RegEx c) (RegEx c)
             | Star (RegEx c)
             

symC :: (Eq c) => c -> RegEx c
symC c = Sym (==c)

alt :: RegEx c -> RegEx c -> RegEx c
alt Null r = r
alt r Null = r
alt r1 r2 = Alt r1 r2

seq :: RegEx c -> RegEx c -> RegEx c
seq Null _ = Null
seq _ Null = Null
seq Eps r = r
seq r Eps = r
seq r1 r2 = Seq r1 r2

star :: RegEx c -> RegEx c 
star Null = Eps
-- don't know if the following is true or not??
-- star Eps = Eps
star r = Star r

empty :: RegEx c -> Bool
empty Null = False
empty Eps = True
empty (Sym _) = False
empty (Alt r1 r2) = empty r1 || empty r2
empty (Seq r1 r2) = empty r1 && empty r2
empty (Star _) = True

fromBool :: Bool -> RegEx c
fromBool True = Eps
fromBool False = Null

derivative :: c -> RegEx c -> RegEx c
derivative _ Null = Null
derivative _ Eps = Null
derivative c (Sym f) = if f c then Eps else Null
derivative c (Alt r1 r2) = alt (derivative c r1) (derivative c r2)
derivative c (Seq r1 r2) = alt (seq (fromBool $ empty r1) (derivative c r2)) 
                               (seq (derivative c r1) (r2))
derivative c (Star r) = seq (derivative c r) (star r)

match :: RegEx Char -> String -> Bool
match r [] = empty r
match r (c:cs) = match (derivative c r) cs




