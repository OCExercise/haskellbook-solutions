module ListPatternMatching where

-- unsafe implementations

head' :: [a] -> a
head' []    = error "empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' []        = error "empty list"
tail' (_:[])    = error "empty list"
tail' (_:xs)    = xs

-- safe implementations

head'' :: [a] -> Maybe a
head'' []     = Nothing
head'' (x:_)  = Just x

tail'' :: [a] -> Maybe [a]
tail'' []     = Nothing
tail'' (_:[]) = Nothing
tail'' (_:xs) = Just xs
