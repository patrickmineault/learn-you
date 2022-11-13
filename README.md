# Exercises from learn you a haskell for great good!

Book available [here](http://learnyouahaskell.com/)

To run the haskell command line tool, use `ghci`. To load a file, use `:l <filename>`. To reload a file, use `:r`. To quit, use `:q`.

# Notes from the book

## List manipulation

`++` concatenates two lists togethers. `:` prepends an element to a list.

Hence, `a : as = [a] ++ as`.

`head` returns the first element of a list. `tail` returns the list without the first element. `last` returns the last element of a list. `init` returns the list without the last element.

In a function declaration, the : operator can be used to pattern match on the head and tail of a list. Hence, `head (x:xs) = x` and `tail (x:xs) = xs`.

## List comprehensions

`[x*2 | x <- [1..10]]` returns a list of the first 10 even numbers.

`[x*2 | x <- [1..10], x*2 >= 12]` returns a list of the first 10 even numbers greater than or equal to 12.

`[x | x <- [50..100], x `mod` 7 == 3]` returns a list of numbers between 50 and 100 that are divisible by 7 and have a remainder of 3 when divided by 7.

## Map and filter

`map` takes a function and a list and applies that function to every element in the list, returning a new list.

`filter` takes a predicate and a list and returns the list of elements that satisfy the predicate.

`map` and `filter` can replicate the functionality of list comprehensions, but sometimes they can be more readable.

## Pattern matching

The _ character is often used to denote a wildcard. Hence: 

```
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _x_ = "Sorry, you're out of luck, pal!"
```

let..in can be used to bind variables in pattern matching. Hence:

```
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  
```

Here,  `let` is used to bind the variables `smallerSorted` and `biggerSorted` to the results of the recursive calls to `quicksort`.

## Infix operators

The function `elem` checks if an element is in a list. For example, `elem 1 [1,2,3]` returns `True`.

Haskell infix operators are functions that take two parameters. For example, `+` is a function that takes two parameters and returns their sum. `+` is an infix operator because it is written between its two parameters. Prefix operators are functions that take one parameter. For example, `not` is a function that takes one parameter and returns its negation. `not` is a prefix operator because it is written before its parameter.

You can get the infix version of a function by surrounding it in backticks. For example, `elem` is the infix version of `elem`. `elem 1 [1,2,3]` is the same as 

```
`1 `elem` [1,2,3]`.
```

You can get the prefix version of an infix function by surrounding it in parentheses. For example, (+) is the prefix version of +. (+) 1 2 is the same as `1 + 2`.

## Currying

Haskell currying allows you to create a function with a parameter baked in. For example `addOne = (+1)` creates a function that takes one parameter and adds one to it. `addOne 2` returns 3.

Haskell functions only take one parameter. What appears to be a function with two parameters is actually a function that takes one parameter and returns a function that takes one parameter. This is the magic of currying. For example the function `add` takes two parameters and returns their sum. `add 1 2` returns 3. `add 1` returns a function that takes one parameter and returns the sum of that parameter and 1. `add 1 2` is the same as `(add 1) 2`.

