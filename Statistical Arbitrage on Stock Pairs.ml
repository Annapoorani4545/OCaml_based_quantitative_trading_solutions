(* Generate mock stock price data using a simple random walk *)
let generate_stock_data n mu sigma =
  let rec aux i prev acc =
    if i >= n then List.rev acc
    else
      let noise = (Random.float (2.0 *. sigma)) -. sigma in
      let new_price = prev +. mu +. noise in
      aux (i + 1) new_price (new_price :: acc)
  in
  Random.self_init ();
  aux 0 100.0 []

(* Compute the mean of a list *)
let mean lst =
  let sum = List.fold_left ( +. ) 0.0 lst in
  sum /. float_of_int (List.length lst)

(* Compute the standard deviation of a list *)
let stddev lst =
  let m = mean lst in
  let variance = List.fold_left (fun acc x -> acc +. (x -. m) ** 2.0) 0.0 lst in
  sqrt (variance /. float_of_int (List.length lst))

(* Calculate the spread between two stocks *)
let calculate_spread stock_a stock_b =
  List.map2 ( -. ) stock_a stock_b

(* Calculate z-score *)
let z_score x mean stddev = (x -. mean) /. stddev

(* Generate trading signals based on z-score threshold *)
let generate_signals spread threshold =
  let m = mean spread in
  let s = stddev spread in
  List.map (fun x ->
    let z = z_score x m s in
    if z > threshold then -1 (* Short Stock A, Long Stock B *)
    else if z < -.threshold then 1 (* Long Stock A, Short Stock B *)
    else 0) spread

(* Main function *)
let () =
  let n = 500 in
  let stock_a = generate_stock_data n 0.1 1.5 in
  let stock_b = generate_stock_data n 0.1 1.5 in

  let spread = calculate_spread stock_a stock_b in
  let threshold = 2.0 in
  let signals = generate_signals spread threshold in

  (* Print the first 20 signals *)
  List.iteri (fun i s -> Printf.printf "Day %d: Signal = %d\n" (i + 1) s) (List.rev (List.tl signals))
