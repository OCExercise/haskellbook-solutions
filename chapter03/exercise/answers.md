# Exercise Solutions

## Reading Syntax

1. Fix or evaluate the following (p. 112-3):
    1. `concact [[1,2,3,],[4,5,6]]` => `[1,2,3,4,5,6]`
    1. `++ [1,2,3] [4,5,6]`
        1. Above errors. Should read: `(++) [1,2,3] [4,5,6] => [1,2,3,4,5,6]`
    1. `(++) "hello" " world"` => `"hello world"`
    1. `["hello" ++ " world]`
        1. Missing closing double-quote. Should read: `["hello" ++ " world"]`
    1. `4 !! "hello"`
        1. Incorrect use of the operator `(!!)`. Should read: `"hello" !! 4`
    1. `(!!) "hello" 4 => `'o'`
    1. `take "4 lovely"`
        1. `take :: Int -> [a] -> [a]`. Should read `take 4 "lovely"`
    1.`take 3 "awesome"` => `"awe"`
1. Match the following (p. 113)
    1. `concat [[1 * 6], [2 * 6], [3 * 6]]` => [6,12,18]
    1. `"rain" ++ drop 2 "elbow"` => `"rainbow"`
    1. `10 * head [1, 2, 3]` => `10`
    1. `(take 3 "Julie") ++ (tail "yes")` => `"Jules"`
    1. `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]` => `[2,3,5,6,8,9]`

## Building
