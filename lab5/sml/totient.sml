fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, a mod b)

fun coprime n =
  let
    fun isCoprime k = gcd (n, k) = 1
  in
    List.filter isCoprime (List.tabulate (n, fn i => i + 1))
  end

fun totient n = List.length (coprime n);


fun factorize 1 _ = []
  | factorize n divisor =
    if n mod divisor = 0 then divisor :: factorize (n div divisor) divisor
    else factorize n (divisor + 1)

fun primeFactors n =
  if n <= 1 then []
  else factorize n 2

fun removeDuplicates [] = []
  | removeDuplicates (x::xs) =
    if List.exists (fn y => y = x) xs then removeDuplicates xs
    else x :: removeDuplicates xs

fun totient2 n =
  let
    val factors = primeFactors n
    val uniqueFactors = removeDuplicates factors
    val phi = Real.fromInt n * List.foldl (fn (p, acc) => acc * (1.0 - 1.0 / Real.fromInt p)) 1.0 uniqueFactors
  in
    Real.round phi
  end


fun timeIt action =
  let
    val start = Time.now()
    val result = action ()
    val end' = Time.now()
    val diff = Time.-(end', start)
  in
    (Time.toReal(diff), result)
  end


fun main () =
  let
    val n = 3411949
    val (time1, result1) = timeIt(fn () => totient n)
    val (time2, result2) = timeIt(fn () => totient2 n)
    val nStr = Int.toString n
  in
    TextIO.print "\n-----\n";
    TextIO.print (concat ["totient ", nStr, " = ", Int.toString result1, "\n"]);
    TextIO.print (concat ["Time for totient ", nStr, " : ", Real.toString time1, "s\n"]);
    TextIO.print "\n";
    TextIO.print (concat ["totient2 ", nStr, " = ", Int.toString result2, "\n"]);
    TextIO.print (concat ["Time for totient2 ", nStr, " : ", Real.toString time2, "s\n"]);
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
