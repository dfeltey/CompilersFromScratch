module Parse where


newtype Parser s t = P ([s] -> [(t,[s])])
unP (P p) = p

pSym :: Eq s => s -> Parser s s
pSym a = P $ \inp -> case inp of 
                     (s:ss) | s == a -> [(s,ss)]
                     otherwise -> []

pReturn :: a -> Parser s a
pReturn a = P $ \inp -> [(a,inp)]

pFail :: Parser s t
pFail = P $ const []

infixl 5 <*>
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
P p1 <*> P p2 = P $ \inp -> [ (v1 v2, ss2) | (v1,ss1) <- p1 inp
                                           , (v2, ss2) <- p2 ss1]

infixr 3 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P $ \inp -> p1 inp ++ p2 inp

pChoice :: [Parser s a] -> Parser s a
pChoice ps = foldr (<|>) pFail ps

infix 7 <$>
(<$>) :: (b -> a) -> Parser s b -> Parser s a
f <$> p = pReturn f <*> p


infixl 3 `opt`
infixl 5 <*, *>
infixl 7 <$

f <$ p = const <$> pReturn f <*> p
p <* q = const <$> p         <*> q
p *> q = id    <$  p         <*> q

opt :: Parser s a -> a -> Parser s a
p `opt` v = p <|> pReturn v


pSatisfy :: (s -> Bool) -> Parser s s
pSatisfy p = P $ \inp -> case inp of
                        (x:xs) | p x -> [(x,xs)]
                        otherwise    -> []

pMany :: Parser s a -> Parser s [a]
pMany p = (:) <$> p <*> pMany p `opt` []

pMany1 :: Parser s a -> Parser s [a]
pMany1 p = (:) <$> p <*> pMany p

applyAll :: a -> [a -> a] -> a
applyAll x [] = x
applyAll x (f:fs) = applyAll (f x) fs

pChainL :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainL op p = applyAll <$> p <*> pMany (flip <$> op <*> p)