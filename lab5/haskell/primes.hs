import Text.Printf


primes :: Int -> [Int]
primes n
    | n < 2     = []
    | otherwise = sieve [2..n]
    where
        sieve :: [Int] -> [Int]
        sieve [] = []
        sieve (p:xs)
            | p * p > n = p : xs
            | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]


main :: IO ()
main = do
    let n = 100
    printf "Prime numbers up to %d: %s\n" n (show $ primes n)
