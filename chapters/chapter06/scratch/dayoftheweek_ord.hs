module DayOfTheWeekOrd where

  data DayOfTheWeek =
      -- disjoint union (sum type) of value symbols
      -- representing days of the week
      Mon | Tue | Wed | Thu | Fri | Sat | Sun


  -- This is looks pretty but is tedious.
  -- I wonder if there's an easier way?
  instance Eq DayOfTheWeek where
      (==) Mon Mon = True
      (==) Tue Tue = True
      (==) Wed Wed = True
      (==) Thu Thu = True
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
