fun sieve [] _ = []
  | sieve (x::xs) n =
    if x * x > n then x :: xs
    else x :: sieve (List.filter (fn y => y mod x <> 0) xs) n

fun primes n =
  if n < 2 then []
  else sieve (List.tabulate (n - 1, fn i => i + 2)) n


fun main () =
  let
    val n = 100
    val nPrimes = primes n
  in
    TextIO.print "\n-----\n";
    TextIO.print ("Prime numbers up to " ^ Int.toString n ^ ": " ^ String.concatWith " " (map Int.toString nPrimes) ^ "\n");
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
