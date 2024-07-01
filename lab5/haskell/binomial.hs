import System.CPUTime
import Text.Printf


binomial :: Int -> Int -> Int
binomial n k
    | k > n = 0
    | k == 0 || k == n = 1
    | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)


pascalsTriangle :: Int -> [Int]
pascalsTriangle 0 = [1]
pascalsTriangle n = zipWith (+) (0 : prevRow) (prevRow ++ [0])
    where
        prevRow = pascalsTriangle (n - 1)

binomial2 :: Int -> Int -> Int
binomial2 n k = (pascalsTriangle n) !! k


timeIt :: IO a -> IO Double
timeIt action = do
    start <- getCPUTime
    _ <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    return diff


main :: IO ()
main = do
    let n = 30
        k = 15

    time1 <- timeIt $ printf "binomial %d %d = %d\n" n k (binomial n k)
    printf "Time for binomial %d %d : %0.3fs\n" n k time1

    printf "\n"

    time2 <- timeIt $ printf "binomial2 %d %d = %d\n" n k (binomial2 n k)
    printf "Time for binomial2 %d %d: %0.3fs\n" n k time2
