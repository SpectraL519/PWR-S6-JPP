fun factorize 1 _ = []
  | factorize n divisor =
    if n mod divisor = 0 then divisor :: factorize (n div divisor) divisor
    else factorize n (divisor + 1)

fun primeFactors n =
  if n <= 1 then []
  else factorize n 2


fun printPrimeFactors n =
  let
    val factors = primeFactors n
  in
    TextIO.print ("Prime factors of " ^ Int.toString n ^ ": " ^ String.concatWith " " (map Int.toString factors) ^ "\n");
    NONE
  end


fun main () =
  let
  in
    TextIO.print "\n-----\n";
    printPrimeFactors 17;
    printPrimeFactors 36;
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
