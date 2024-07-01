fun extGcd a 0 = (1, 0, a)
  | extGcd a b =
    let
      val (x', y', gcdAB) = extGcd b (a mod b)
      val (x, y) = (y', x' - y' * (a div b))
    in
      (x, y, gcdAB)
    end;

fun de (a, b) = extGcd a b;


fun main () =
  let
    val (a, b) = (30, 42)
    val (x, y, z) = de (a, b)
    val aStr = Int.toString a
    val bStr = Int.toString b
    val xStr = Int.toString x
    val yStr = Int.toString y
    val zStr = Int.toString z
  in
    TextIO.print "\n-----\n";
    TextIO.print (concat ["Solution to the equation ", aStr, "x + ", bStr, "y = gcd(", aStr, ", ", bStr, "):\n"]);
    TextIO.print (concat ["x = ", xStr, " | y = ", yStr, " | gcd(", aStr, ", ", bStr, ") = ", zStr, "\n"]);
    TextIO.print (concat ["Check: ", aStr, " * ", xStr, " + ", bStr, " * ", yStr, " = ", Int.toString (a * x + b * y), "\n"]);
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
