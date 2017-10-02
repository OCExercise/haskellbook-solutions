module Identity where

data Identity a = Identity a

-- Can you guess the problem here? Hint,
-- do we know enough about the type of a
-- to perform the operation (==) against
-- values of that type?
instance Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
