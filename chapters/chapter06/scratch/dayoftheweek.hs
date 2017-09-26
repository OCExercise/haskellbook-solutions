module DayOfTheWeek where

data DayOfTheWeek =
    -- disjoint union (sum type) of value symbols
    -- representing days of the week
    Mon | Tues | Wed | Thurs | Fri | Sat | Sun


-- This is looks pretty but is tedious.
-- I wonder if there's an easier way?
instance Eq DayOfTheWeek where
    (==) Mon Mon = True
    (==) Tues Tues = True
    (==) Wed Wed = True
    (==) Thurs Thurs = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date = Date DayOfTheWeek Int

instance Eq Date where
  (==)  (Date weekday dayOfTheMonth)
        (Date weekday' dayOfTheMonth') =
        weekday == weekday'
    &&  dayOfTheMonth == dayOfTheMonth'
