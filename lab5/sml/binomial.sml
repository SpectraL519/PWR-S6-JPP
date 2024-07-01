fun binomial n k =
  if k > n then 0
  else if k = 0 orelse k = n then 1
  else binomial (n - 1) k + binomial (n - 1) (k - 1)


fun pascalsTriangle 0 = [1]
  | pascalsTriangle n =
    let
      val prevRow = pascalsTriangle (n - 1)
    in
      ListPair.map (fn (x, y) => x + y) (0::prevRow, prevRow@[0])
    end

fun binomial2 n k = List.nth(pascalsTriangle n, k)


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
    val n = 32
    val k = 16
    val nStr = Int.toString n
    val kStr = Int.toString k
    val (time1, result1) = timeIt(fn () => binomial n k)
    val (time2, result2) = timeIt(fn () => binomial2 n k)
  in
    TextIO.print "\n-----\n";
    TextIO.print (concat ["binomial ", nStr, " ", kStr, " = ", Int.toString result1, "\n"]);
    TextIO.print (concat ["Time for binomial ", nStr, " ", kStr, " : ", Real.toString time1, "s\n"]);
    TextIO.print (concat ["\nbinomial2 ", nStr, " ", kStr, " = ", Int.toString result2, "\n"]);
    TextIO.print (concat ["Time for binomial2 ", nStr, " ", kStr, " : ", Real.toString time2, "s\n"]);
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
