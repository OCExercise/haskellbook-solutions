module InstanceDispatch where

-- We declare functions attached to this type
-- class here, but we do not define them
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

-- Our first introduction to "newtype."
-- As the text suggests, just pretend this is
-- data. We'll get to subtyping in Chapter 11
newtype Age = Age Integer deriving (Eq, Show)

-- We implement functions associated with
-- in instances, where they can be bound to monomorphic
-- or constrained polymorphic types. Here, fromNumber
-- and toNumber are defined for the type Age.
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

-- Another subtype
newtype Year = Year Integer deriving (Eq, Show)

-- We now defined fromNumber and toNumber for
-- the type Year.
instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

-- And here's a function in the module global
-- namespace that operates types with instances
-- under Numberish
sumNumberish :: Numberish a => a -> a -> a
sumNumberish x y = fromNumber summed
  where integerOfX  = toNumber x
        integerOfY  = toNumber y
        summed      = integerOfX + integerOfY
