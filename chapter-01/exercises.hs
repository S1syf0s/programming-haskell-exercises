-- 1.

{-

double (double 2)
(double 2) + (double 2)
(double 2) + (2 + 2)
(2 + 2) + (2 + 2)
4 + 4
8

-}

-- 2.

{-

sum [x] = x
x + sum []
x + 0
x

-}

-- 3.

{-

times [2, 3, 4]
2 * (times [3 , 4])
2 * ( 3 * (times [4]))
2 * (3 * ( 4 * (time [])))
2 * (3 * ( 4 * (1)))
24

-}

-- 4.

{-

Qsort is supposed to sort list given as argument and return sorted list.
Fuction should be modified as follows for it to be able to return reverse
sorted list.

-}

qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]


reverse_qsort []      = []
reverse_qsort  (x:xs) = reverse_qsort larger ++ [x] ++ reverse_qsort smaller
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]

-- 5.

modified_qsort []     = []
modified_qsort (x:xs) = modified_qsort smaller ++ [x] ++ modified_qsort larger
               where smaller = [a | a <- xs, a < x]
                     larger = [b | b <- xs, b > x]

{-

If we modify the function like this and give list [2,2,3,1,1] as argument,
Haskell will not preserve elements that are not handled by the code.
Thus list comprehensions work like filters, they only include elements that
satisfy the conditions specified in the code.

-}

