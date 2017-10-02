module DayOfTheWeek where

data DayOfTheWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun


-- Let's omit the last case to break this
instance Eq DayOfTheWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    -- (==) _ _ = False
