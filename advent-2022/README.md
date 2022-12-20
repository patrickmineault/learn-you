# Advent of Code 2022

Here I'm writing what I'm learning from the [Advent of Code 2022](https://adventofcode.com/2022) puzzles.

## Using custom types

Haskell programmers sure love custom types. `type` is used to declare a type synonym. For example:

```haskell
type String = [Char]
```

`data` is used to declare a new data type. For example:

```haskell
data Bool = False | True
```

`data` can also be used to declare a new type with parameters. For example:

```haskell
data Point = Point Float Float
```

This declares a new type `Point` with two parameters `Float` and `Float`. The constructor `Point` is used to create a new `Point` object. For example:

```haskell
origin = Point 0 0
```

The `Point` constructor can also be used to pattern match. For example:

```haskell
surface :: Point -> Float
surface (Point x y) = x * y
```

## Instance

`instance` is used to declare a new typeclass instance. For example:

```haskell
instance Eq Point where
    (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2
```

This declares a new instance of the `Eq` typeclass for the `Point` type. The `Eq` typeclass is used to test for equality. The `==` function is used to test for equality. The `==` function is defined in the `Eq` typeclass. The `==` function is defined for all types that are instances of the `Eq` typeclass. The `==` function is defined for the `Point` type by pattern matching on the `Point` constructor.

Some common typeclasses include:

- `Eq` - equality
- `Ord` - ordering
- `Show` - string representation
- `Read` - string to type
- `Enum` - sequential types
- `Bounded` - bounded types
- `Num` - numeric types
- `Integral` - integral types
- `Floating` - floating point types
- `Functor` - functor
- `Monad` - monad

The `Read` typeclass is used to convert a string to a type. For example:

```haskell
read "True" || False
read "8.2" + 3.8
read "5" - 2
read "[1,2,3,4]" ++ [3]
read "(3, 'a')" :: (Int, Char)
```

To implement the `Read` typeclass for a custom type, you have to implement the `readsPrec` function. For example:

```haskell
instance Read Point where
    readsPrec _ value =
        tryParse [("Point", parsePoint)]
        where
            tryParse [] = []
            tryParse ((attempt, parse):xs) =
                if (take (length attempt) value) == attempt
                    then parse (drop (length attempt) value)
                    else tryParse xs
            parsePoint value =
                [(Point x y, rest) |
                    ("(", rest) <- lex value,
                    (x, rest) <- reads rest,
                    (",", rest) <- lex rest,
                    (y, rest) <- reads rest,
                    (")", rest) <- lex rest]
```

The `readsPrec` function takes a `Int` and a `String` and returns a list of tuples. The first element of the tuple is the parsed value and the second element is the remaining string. The `readsPrec` function is used by the `read` function. The `read` function takes a `String` and returns a value. The `read` function is defined in the `Read` typeclass. The `read` function is defined for all types that are instances of the `Read` typeclass. The `read` function is defined for the `Point` type by calling the `readsPrec` function.

Here, `lex` is used to split a string into a list of tokens. For example:

```haskell
lex "Point (1, 2)"
-- [("Point", " (1, 2)")]
```

