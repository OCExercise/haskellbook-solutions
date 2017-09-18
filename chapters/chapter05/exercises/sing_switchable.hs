module SingSwitchable where

fstString' :: [Char] -> [Char]
fstString' x = x ++ " in the rain"

sndString' :: [Char] -> [Char]
sndString' x = x ++ " over the rainbow"

sing' :: ([Char] -> [Char] -> Bool) -> [Char]
sing' op =
    if (x `op` y) then fstString' x else sndString' y
        where
            x = "Singin"
            y = "Somewhere"
