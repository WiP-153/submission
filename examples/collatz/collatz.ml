(* ./mixc examples/collatz/collatz.ml -arg "(4,1);(3,2);(7,6);(1,4);(2,2);(3,2);(2,3)" *)

let collatz(x) =
  let rec loop(n,t) =
    if n == 1 then t else 
    if (n mod 2) == 0 then
      loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(x,1) ;;

let max (t1,t2) = if t1 > t2 then t1 else t2 ;;

let main ((x,y) : (int<8> * int<8>)) : int<8> =
  let (t1,_) = exec collatz(x) default 0 in
  let (t2,_) = exec collatz(y) default 0 in
  max(t1,t2) ;;