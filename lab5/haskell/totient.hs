import System.CPUTime
import Text.Printf
import Data.List (group)


coprime :: Int -> [Int]
coprime n = [k | k <- [1..n], gcd n k == 1]

totient :: Int -> Int
totient n = length $ coprime n


factorize :: Int -> Int -> [Int]
factorize 1 _ = []
factorize n divisor
    | n `mod` divisor == 0 = divisor : factorize (n `div` divisor) divisor
    | otherwise            = factorize n (divisor + 1)

primeFactors :: Int -> [Int]
primeFactors n
    | n <= 1    = []
    | otherwise = factorize n 2

totient2 :: Int -> Int
totient2 n =
    let factors = primeFactors n
        uniqueFactors = map head $ group factors
    in round $ fromIntegral n * product [1 - 1 / fromIntegral p | p <- uniqueFactors]


timeIt :: IO a -> IO Double
timeIt action = do
    start <- getCPUTime
    _ <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    return diff


main :: IO ()
main = do
    let n = 39916801

    time1 <- timeIt $ printf "totient %d = %d\n" n (totient n)
    printf "Time for totient %d : %0.3fs\n" n time1

    printf "\n"

    time2 <- timeIt $ printf "totient2 %d = %d\n" n (totient2 n)
    printf "Time for totient2 %d : %0.3fs\n" n time2
