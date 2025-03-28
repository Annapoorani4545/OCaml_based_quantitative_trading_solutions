(* Define a record for stock data *)
type stock_data = {
  date : string;
  price : float;
}

(* Implement List.take manually *)
let rec take n lst =
  match lst, n with
  | _, 0 -> []
  | [], _ -> failwith "List.take: not enough elements"
  | x :: xs, _ -> x :: take (n-1) xs

(* Calculate the simple moving average (SMA) *)
let moving_average data window_size =
  let rec moving_avg_helper data window_size acc =
    match data with
    | [] -> List.rev acc
    | _ when List.length data < window_size -> List.rev acc
    | _ ->
      let window = List.fold_left (fun sum d -> sum +. d.price) 0.0 (List.tl (List.rev (take window_size data))) in
      let avg = window /. float_of_int window_size in
      moving_avg_helper (List.tl data) window_size (avg :: acc)
  in
  moving_avg_helper data window_size []

(* Generate buy/sell signals based on SMA crossover *)
let generate_signals short_ma long_ma data =
  let rec aux short_ma long_ma data prev_signal acc =
    match (short_ma, long_ma, data) with
    | (s::ss, l::ls, d::ds) ->
      let signal =
        if s > l && prev_signal <> "BUY" then "BUY"
        else if s < l && prev_signal <> "SELL" then "SELL"
        else "HOLD"
      in
      aux ss ls ds signal ((d.date, signal) :: acc)
    | _ -> List.rev acc
  in
  aux short_ma long_ma data "HOLD" []

(* Simulate backtesting *)
let backtest signals data =
  let rec aux signals data position balance =
    match (signals, data) with
    | ((_, "BUY")::ss, d::ds) -> aux ss ds 1 (balance -. d.price)
    | ((_, "SELL")::ss, d::ds) when position = 1 -> aux ss ds 0 (balance +. d.price)
    | (_::ss, _::ds) -> aux ss ds position balance
    | _ -> balance
  in
  aux signals data 0 10000.0 (* Starting with 10,000 units *)

(* Example data for testing *)
let example_data = [
  {date="2025-03-20"; price=100.0};
  {date="2025-03-21"; price=102.0};
  {date="2025-03-22"; price=101.0};
  {date="2025-03-23"; price=104.0};
  {date="2025-03-24"; price=98.0};
]

let () =
  let short_ma = moving_average example_data 2 in
  let long_ma = moving_average example_data 3 in
  let signals = generate_signals short_ma long_ma example_data in
  let final_balance = backtest signals example_data in
  List.iter (fun (date, signal) -> Printf.printf "%s: %s\n" date signal) signals;
  Printf.printf "Final Balance: %.2f\n" final_balance
