module PrintableNumberDispatch where

class PrintableNumber a where
  convertNumberToString :: a -> String

newtype Integerish = Integerish Integer deriving (Eq, Show)
newtype Doublish = Doublish Double deriving (Eq, Show)

instance PrintableNumber Integerish where
  convertNumberToString x = show x

instance PrintableNumber Doublish where
  convertNumberToString x = show x

concatPrintableNumbers :: PrintableNumber a => a -> a -> String
concatPrintableNumbers x y = concat [(convertNumberToString x), ", ", (convertNumberToString y)]
