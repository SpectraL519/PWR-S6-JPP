fun merge xs [] = xs
  | merge [] ys = ys
  | merge (x::xs) (y::ys) =
    if x <= y then x :: merge xs (y::ys)
    else y :: merge (x::xs) ys

fun halve xs =
  let
    val len = length xs
    val mid = len div 2
  in
    (List.take(xs, mid), List.drop(xs, mid))  (* Wrap the tuple in parentheses *)
  end

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
    let
      val (left, right) = halve xs
    in
      merge (mergesort left) (mergesort right)
    end


fun main () =
  let
    val unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
    val sortedList = mergesort unsortedList
  in
    TextIO.print "\n-----\n";
    TextIO.print (String.concatWith " " (map Int.toString unsortedList) ^ "\n");
    TextIO.print (String.concatWith " " (map Int.toString sortedList) ^ "\n");
    TextIO.print "-----\n\n";
    NONE
  end

val _ = main ()
