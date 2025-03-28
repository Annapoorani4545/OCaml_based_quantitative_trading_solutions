(* Define the state for the moving average and orders *)
type state = {
  prices: float list;
  window_size: int;
  spread: float;
}

(* Calculate the simple moving average *)
let sma prices =
  let sum = List.fold_left (+.) 0.0 prices in
  sum /. float_of_int (List.length prices)

(* Update state with a new price *)
let update_state state new_price =
  let updated_prices = new_price :: (if List.length state.prices >= state.window_size then List.tl state.prices else state.prices) in
  { state with prices = updated_prices }

(* Generate buy/sell orders based on SMA and spread *)
let generate_orders state =
  match state.prices with
  | [] | [_] -> None (* Not enough data *)
  | _ ->
    let moving_avg = sma state.prices in
    let buy_price = moving_avg -. state.spread /. 2.0 in
    let sell_price = moving_avg +. state.spread /. 2.0 in
    Some (buy_price, sell_price)

(* Simulate a stream of prices and run the algorithm *)
let rec simulate prices state =
  match prices with
  | [] -> ()
  | price :: rest ->
    let state = update_state state price in
    match generate_orders state with
    | None -> print_endline "Not enough data to place orders."
    | Some (buy_price, sell_price) ->
      Printf.printf "Price: %.2f, Buy Order: %.2f, Sell Order: %.2f\n" price buy_price sell_price;
    simulate rest state

(* Example simulation *)
let () =
  let initial_state = { prices = []; window_size = 5; spread = 1.0 } in
  let price_data = [100.0; 101.0; 102.0; 101.5; 100.5; 103.0; 104.0; 102.5; 101.0] in
  simulate price_data initial_state

