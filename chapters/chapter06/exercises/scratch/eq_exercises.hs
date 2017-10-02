module EqExercises where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = v == v'
