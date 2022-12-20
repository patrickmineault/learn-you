# Exercises from learn you a haskell for great good!

Book available [here](http://learnyouahaskell.com/)

To run the haskell command line tool, use `ghci`. To load a file, use `:l <filename>`. To reload a file, use `:r`. To quit, use `:q`.

# Basics

To write a comment in haskell, use:
    
    ```haskell
    -- This is a comment
    ```

The basic division operator `/` is floating point division. To do integer division, use `div`.

    ```haskell
    div 10 3 -- 3
    ```

You can define an infix operator with only special characters like this:
    
    ```haskell
    infixr 5 :-:
    data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
    ```

The `foldr` function takes a function, a starting value, and a list to fold up. It applies the function to the starting value and the head of the list, then applies the function to the result and the second element of the list, and so on. It's like `reduce` in javascript.
    
    ```haskell
    foldr (+) 0 [1,2,3,4] -- 10
    ```

The `foldl` function is like `foldr`, but it folds from the left instead of the right. It's like `reduceRight` in javascript.


    
The function `zip` takes two lists and zips them together into one list of pairs. If one input list is short, excess elements of the longer list are discarded.
    
    ```haskell
    zip [1,2,3,4,5] [5,5,5,5,5] -- [(1,5),(2,5),(3,5),(4,5),(5,5)]
    ```

It's similar to Python's `zip` function. On the other hand, `zipWith` takes a function and two lists and then joins the two lists by applying the function between corresponding elements.
    
    ```haskell
    zipWith (+) [1,2,3,4,5] [5,5,5,5,5] -- [6,7,8,9,10]
    ```

The operator !! takes a list and a number and returns the element at that index in the list. Lists are 0-indexed, so the first element is at index 0.
    
    ```haskell
    [1,2,3,4,5] !! 2 -- 3
    ```

To take a list and represent it as a string, use `show`.
    
    ```haskell
    show [1,2,3,4,5] -- "[1,2,3,4,5]"
    ```

# Math and complex numbers

The `Complex` type is defined in the `Data.Complex` module. First, you have to import it:
    
    ```haskell
    import Data.Complex
    ```

Not that this is an unqualified import. This means that all the functions and types from the module are available in the global namespace. If you want to import only a few functions, you can use a qualified import:
    
    ```haskell
    import qualified Data.Complex as C
    ```

It's a type that represents complex numbers. It's defined like this:
    
    ```haskell
    data Complex a = !a :+ !a
    ```

To create a complex number, use the `:+` operator.
    
    ```haskell
    3 :+ 4 -- 3.0 :+ 4.0
    ```

The part after `:+` corresponds to the imaginary part of the number. The part before `:+` corresponds to the real part of the number. You can also use the `complex` function to create a complex number.
    
    ```haskell
    complex 3 4 -- 3.0 :+ 4.0
    ```

To add two complex numbers a and b together, use the `+` operator.
    
    ```haskell
    3 :+ 4 + 5 :+ 6 -- 8.0 :+ 10.0
    ```

To multiply two complex numbers a and b together, use the `*` operator.
    
    ```haskell
    3 :+ 4 * 5 :+ 6 -- -9.0 :+ 38.0
    ```

The natural exponential function is defined in the `Data.Complex` module. It's called `cis`. It takes an angle in radians and returns the complex number that corresponds to that angle.
    
    ```haskell
    cis (pi / 4) -- 0.7071067811865475 :+ 0.7071067811865475
    ```

`pi` is defined in the `Prelude` module; you don't need to specifically import it.

The imaginary unit is defined in the `Data.Complex` module. It's called `i`. It's equal to `0 :+ 1`.
    
    ```haskell
    i -- 0.0 :+ 1.0
    ```

To divide by an integer, use the `fromIntegral` function. It takes an integer and returns a floating point number.
    
    ```haskell
    fromIntegral 3 -- 3.0
    ```

# Notes from the book

## List manipulation

`++` concatenates two lists togethers. `:` prepends an element to a list.

Hence, `a : as = [a] ++ as`.

`head` returns the first element of a list. `tail` returns the list without the first element. `last` returns the last element of a list. `init` returns the list without the last element. 

In a function declaration, the : operator can be used to pattern match on the head and tail of a list. Hence, `head (x:xs) = x` and `tail (x:xs) = xs`.

To concatenate two lists, use `++`. To prepend an element to a list, use `:`.

To calculate the fft of a polynomial using a recursive function, you can use the following pattern:
    
    ```haskell
    fft :: [Complex Double] -> [Complex Double]
    fft [] = []
    fft [x] = [x]
    fft xs = zipWith (+) (fft evens) (map (*w) (fft odds))
        where
            n = length xs
            k = [0..n-1]
            evens = [xs !! i | i <- k, even i]
            odds = [xs !! i | i <- k, odd i]
            w = cis (-2 * pi / fromIntegral n)
    ```

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

## Sets

Sets are defined in the library Data.Set. One can get 

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

## Hashmaps

Haskell has a `Data.Map` module which implements a hashmap. To create a hashmap:

```
import qualified Data.Map as Map
phoneBook = [("betty","555-2938"),("bonnie","452-2928"),("patsy","493-2928")]
theMap = Map.fromList phoneBook
```

To index into this map, use `Map.lookup`. For example, `Map.lookup "betty" theMap` returns `Just "555-2938"`, whereas `Map.lookup "bob" theMap` returns `Nothing`.

The Python `in` operator translates in Haskeel to `Map.member`. It can be emulated using this function:

```
member' :: (Eq k, Ord k) => k -> Map.Map k a -> Bool
member' key theMap = case Map.lookup key theMap of
    Nothing -> False
    Just _ -> True
```

The Python dict methods are similar to Haskell's Map module. Here's a list of corresponding methods:

| Description | Python | Haskell |
| --- | --- | --- |
| Create a new dictionary | `d = {}` | `d = Map.empty` |
| Fill a dictionary with values | `d = {"a": 1, "b": 2}` | `d = Map.fromList [("a", 1), ("b", 2)]` |
| Get the value of a key | `d["a"]` | `Map.lookup "a" d` |
| Check if a key is in a dictionary | `"a" in d` | `Map.member "a" d` |
| Add a key-value pair to a dictionary | `d["c"] = 3` | `d = Map.insert "c" 3 d` |
| Remove a key-value pair from a dictionary | `del d["a"]` | `d = Map.delete "a" d` |
| Get the keys of a dictionary | `d.keys()` | `Map.keys d` |
| Get the values of a dictionary | `d.values()` | `Map.elems d` |
| Get the key-value pairs of a dictionary | `d.items()` | `Map.toList d` |


## <$>

The `<$>` operator is a function that takes a function and a functor and applies the function to the contents of the functor. For example, `(+1) <$> [1,2,3]` returns `[2,3,4]`. `(+1) <$> Just 1` returns `Just 2`. `(+1) <$> Nothing` returns `Nothing`.