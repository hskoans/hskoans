# IO

(1) Compile executable program

```
ghc HelloWorld.hs
./HelloWorld
```

How can this be pure if it modifies an external state, i.e. write to console?

(2) IO Actions

In ghci, `:t putStrLn` gives us `putStrLn :: String -> IO ()`

`()` in Haskell is equivalent to "void" in C/C++ etc. It is of the type `data Unit = Unit`
`IO` is a parametrized type, which is a type variable that can hold any other type. Similar to `Maybe`.

In other words, `putStrLn` returns an `IO ()` type.

`main` is special keyword recognized by Haskell and of type `IO ()`. It is IO action executed by the program.

For instance,

```
main :: IO ()
main = putStrLn "Hello World" -- printed

main2 :: IO ()
main2 = putStrLn "Hello World 2" -- no printed
```

(3) Combine IO Actions

Use do-blocks to combine IO actions:

```
main :: IO ()
main = do
    putStrLn "Hello"
    putStrLn "World"
x = 3 -- no longer part of the do block
```

This will print "Hello World" 3 times.

```
helloWorld :: IO ()
helloworld = putStrLn "Hello World"

main :: IO ()
main = do
    helloWorld
    helloWorld
    helloWorld
```

Another example:

```
introduce :: String -> String -> IO ()
introduce name1 name2 = do
    putStrLn (name1 ++ ", this is " ++ name2)
    putStrLn (name2 ++ ", this is " ++ name1)

main :: IO ()
main = do
    introduce "Alice" "Bob"
    introduce "Alice" "Sally"
```

(4) IO Values

```
main :: IO ()
main = do
    line <- getLine
    putStrLn ("You said: " ++ line)
```

In ghci, we can get:

```
:t getLine
getLine :: IO String
```
`<-` binds the result of the IO action to the `line` variable.
`<-` only works inside do-blocks.

Here's another example:

```
greet :: IO ()
greet = do
    putStrLn "Who are you?"
    who <- getLine
    putStrLn ("Hello " ++ who)

greetForever :: IO ()
greetForever = do
    greet
    greetForever -- recursive call

main :: IO ()
main = greetForever
```

This *CANNOT* be implemented because it is impure:

```
extractValue :: IO a -> a
```

The only exception is `unsafePerformIO`. We should not use it at all!

(5) Useful IO Functions

`return` function:

```
dummyGetLine :: IO String
dummyGetLine =
    return "I'm not really doing anything."

main :: IO ()
main = do
    line <- dummyGetLine
    putStrLn line
```

In ghci:

```
return :: a -> IO a
```

Example:

```
promptInfo :: IO (String, String)
promptInfo = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "What is your favorite color?"
    color <- getLine
    return (name, color)

main :: IO ()
main = do
    (name, color) <- promptInfo
    putStrLn ("Hello " ++ name)
    putStrLn ("I like " ++ color ++ " too!")
```

This is wrong:

```
main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    lines <- line1 ++ line2 -- This is wrong as `<-` is expecting an IO action on the right
    putStrLn lines
```

This is correct:

```
main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    lines <- return (line1 ++ line2) -- because `return` creates an IO action
    putStrLn lines
```

But the idiomatic way is to do this:

```
main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    let lines = line1 ++ line2
    putStrLn lines
```

Don't confuse `return` with the `return` that we are familiar with in traditional languages. In Haskell, `return` gives us an IO action and does not prevent lines after it from running.

```
main :: IO ()
main = do
    return 0 -- produces 0 but throws it away since we don't bind it to any var
    putStrLn "haha, still running"
    return "halt!" -- produces "halt!" but throws it away since we don't bind it to any var
    putStrLn "you can't stop me!"
```

Everything will execute so we can't control the flow of the program unless we use monads, which we will discuss later.

List of useful IO actions:

`putStrLn :: String -> IO ()`

prints a string to the console and append a new line.

`getLine :: IO String`

reads a line from the console.

`print :: (Show a) => a -> IO ()`

print string representation of a value.

`readFile :: FilePath -> IO String`

read an entire file as a (lazy) string.

`writeFile :: FilePath -> String -> IO ()`

write a string to a file.

`appendFile :: FilePath -> String -> IO ()`

appends a string to a file.
`FilePath` is simply a type alias to `String`, i.e. `type FilePath = String`

`interact :: (String -> String ) -> IO ()`

Example usage of `interact`:

```
reverseLines :: (String -> String) -> IO ()
reverseLines input =
    unlines (map reverse (lines input))

main :: IO ()
main = interact reverseLines
```

(6) Program Organization

Example program that is written in "traditional" programming style.

```
encrypt :: Char -> Char
encrypt = ... -- incomplete function, just for illustration purposes

handleChar :: IO ()
handleChar = do
    c <- getChar
    let u = encrypt c
    putChar c

inputLoop :: IO ()
inputLoop = do
    handleChar
    inputLoop

main :: IO ()
main = inputLoop
```

This is a better way to do it, Haskell way.

```
encrypt :: Char -> Char
encrypt = ... -- incomplete function, just for illustration purposes

main ::  IO ()
main = interact (map encrypt)
```

