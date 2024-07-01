import Text.Printf


factorize :: Int -> Int -> [Int]
factorize 1 _ = []
factorize n divisor
    | n `mod` divisor == 0 = divisor : factorize (n `div` divisor) divisor
    | otherwise            = factorize n (divisor + 1)

primeFactors :: Int -> [Int]
primeFactors n
    | n <= 1    = []
    | otherwise = factorize n 2


printPrimeFactors :: Int -> IO ()
printPrimeFactors n = do
    printf "Prime factors of %d: %s\n" n (show $ primeFactors n)


main :: IO ()
main = do
    printPrimeFactors 17
    printPrimeFactors 36
