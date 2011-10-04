p4 :: Int -> Int
p4 n = maximum [i*j | i <- [1..(10^n)-1], j <- [1..(10^n)-1], isPalin (i*j)]

isPalin :: Int -> Bool
isPalin i = show i == reverse (show i)

main = p4 3