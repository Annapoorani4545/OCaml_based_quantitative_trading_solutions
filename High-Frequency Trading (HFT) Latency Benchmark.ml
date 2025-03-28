(* Generate random price data *)
let generate_prices n =
  Array.init n (fun _ -> (Random.float 1000.0, Random.int 100))

(* Simple trading strategy: buy if price < 500, sell otherwise *)
let trading_strategy (price, volume) =
  if price < 500.0 then
    Some ("BUY", volume)
  else
    Some ("SELL", volume)

(* Simulate trading with latency measurement *)
let simulate_trading n =
  let prices = generate_prices n in
  let start_time = Sys.time () in
  let trades = Array.map trading_strategy prices in
  Array.iter (fun trade -> match trade with
    | Some (action, volume) -> Printf.printf "%s %d\n" action volume
    | None -> ()) trades;
  let end_time = Sys.time () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Executed %d trades in %.6f seconds\n" n elapsed_time

(* Main function to run the benchmark *)
let () =
  Random.self_init ();
  let trade_count = 1_000_000 in
  simulate_trading trade_count
