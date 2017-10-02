module Identity where

data Identity a = Identity a

-- We can use a type class constraints
-- in instance declarations, ensuring that
-- compiler or REPL understand that the operation
-- (==) applies to values of type a (v, v')
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
