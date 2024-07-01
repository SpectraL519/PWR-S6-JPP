import Text.Printf


extGcd :: Int -> Int -> (Int, Int, Int)
extGcd a 0 = (1, 0, a)
extGcd a b =
    let (x', y', gcdAB) = extGcd b (a `mod` b)
        (x, y) = (y', x' - y' * (a `div` b))
    in (x, y, gcdAB)

de :: (Int, Int) -> (Int, Int, Int)
de (a, b) = extGcd a b


main :: IO ()
main = do
    let (a, b) = (30, 42)
        (x, y, z) = de (a, b)

    printf "Solution to the equation %dx + %dy = gcd(%d, %d):\n" a b a b
    printf "x = %d | y = %d | gcd(%d, %d) = %d\n" x y a b z
    printf "Check: %d * %d + %d * %d = %d\n" a x b y (a * x + b * y)
