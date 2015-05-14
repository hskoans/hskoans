string1 = "hello"
string2 = "world"
greeting = string1 ++ " " ++ string2

-- simple functions

square x = x * x

multMax a b x = (max a b) * x

posOrNeg x =
    if x >= 0
    then "Positive"
    else "Negative"

-- recursion

-- pow2 n = 2 to the power of n
pow2 n =
    if n == 0
    then 1
    else 2 * (pow2 (n-1))

repeatString str n =
    if n == 0
    then ""
    else str ++ (repeatString str (n - 1))

pow2' n = pow2loop n 1 0
pow2loop n x i =
    if i < n
    then pow2loop n (x * 2) (i + 1)
    else x

-- lists
x = [1, 2, 3]
empty = []
y = 0 : x  -- `:` is used to append to a list
x' = 1 : (2 : ( 3: []))
x'' = 1 : 2 : 3 : []

-- strings are essentially lists
str = "abcde"
str' = 'a' : 'b' : 'c' : 'd' : 'e' : []

-- accessing lists
h = head [1, 2, 3]
t = tail [1, 2, 3]
ht = head (tail [1, 2, 3])
tt = tail (tail [1, 2, 3])
htt = head (tail (tail [1, 2, 3]))

-- is our list empty?
isEmpty = null []
isEmpty' = null [1, 2]

-- operating on a list
double nums =
    if null nums
    then []
    else (2 * (head nums)) : (double (tail nums))

removeOdd nums =
    if null nums
    then []
    else
        if (mod (head nums) 2) == 0 -- even?
        then (head nums) : (removeOdd (tail nums))
        else removeOdd (tail nums)

-- tuples
a = (1, "hello")
b = ("pi", 3.14159, [1, 2, 3], "four")

-- comparison between tuples and lists
-- tuples               lists
-- (...)                [...]
-- different types      same type
-- fixed length         unbounded length (we can keep adding elements)

-- tuples are useful for returning multiple values from a function
headAndLength list = (head list, length list)

-- built-in functions that help us access tuple elements
firstEl = fst (1, "hello")
secondEl = snd (1, "hello")

-- A warning about the use of tuples
-- when tuples are big (more than 3 or 4 elements) in it
-- or when tuples are used around different parts of an application
-- it's usually better to use a custom datatype instead of tuple

-- Pattern matching
fst' (a, b) = a
snd' (a, b) = b

-- Pattern matching lists
-- null' is our function name that handles different patterns
null' [] = True
null' (x : xs) = False

head' (x : xs) = x
head' [] = error "head of empty list" -- program will crash but the error message will show

-- rewriting our earlier function with pattern matching
{- double nums =
    if null nums
    then []
    else (2 * (head nums)) : (double (tail nums)) -}

double' [] = []
double' (x : xs) = (2 * x) : (double xs)

-- case expressions (pattern match at some other point in the function)
-- implementing the same double function
double'' nums = case nums of
    [] -> []
    (x : xs) -> (2 * x) : (double xs)

-- another case expression and this cannot be easily implemented as 
-- a pure pattern matching function
anyEven nums = case (removeOdd nums) of
    [] -> False
    (x : xs) -> True

-- Note that no guards are permitted on the case expression
-- use if-then if we need to handle scenarios in the case expression

-- let binding 
fancySeven =
    let a = 3
    in 2 * a + 1

fancyNine =
    let x = 4
        y = 5
    in x + y

numEven nums =
    let evenNums = removeOdd nums
    in length evenNums

-- where cannot be used inside an expression
-- fancyTen = 2 * (a + 1 where a = 4) <-- This will not compile

fancyTen = 2 * (let a = 4 in a + 1)

-- where is top down
-- let is bottom up

pow2'' n
    | n == 0 = 1
    | otherwise = 2 * (pow2'' (n - 1))

removeOdd' [] = []
removeOdd' (x : xs)
    | mod x 2 == 0 = x : (removeOdd' xs)
    | otherwise = removeOdd' xs

-- lazy evaluation
-- lazy infinite lists
intsFrom n = n : (intsFrom (n + 1))
ints = intsFrom 1   -- ints is an infinite list of integers, but it doesn't crash
                    -- since it's not evaluated
-- we can evaluate parts of this infinite list with no problems
ans = head ints
evenInts = removeOdd ints
ans2 = take 10 evenInts
