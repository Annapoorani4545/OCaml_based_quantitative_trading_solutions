(* Define types for representing stock data and factors *)
type stock_data = {
  ticker : string;
  date : string;
  size : float;
  value : float;
  momentum : float;
  returns : float;
}

type factor_data = {
  size_premium : float;
  value_premium : float;
  momentum_premium : float;
}

(* Calculate expected returns using Fama-French factors *)
let calculate_expected_return (stock : stock_data) (factor : factor_data) =
  stock.size *. factor.size_premium +.
  stock.value *. factor.value_premium +.
  stock.momentum *. factor.momentum_premium

(* Generate trading signals based on expected returns *)
let generate_signal (stock : stock_data) (factor : factor_data) =
  let expected_return = calculate_expected_return stock factor in
  if expected_return > 0.0 then "BUY" else "SELL"

(* Print stock data to avoid warnings *)
let print_stock_data (stock: stock_data) =
  Printf.printf "Processing stock: %s on date: %s\n" stock.ticker stock.date

(* Backtesting the strategy *)
let backtest (data : stock_data list) (factor : factor_data) =
  let rec loop data total_return =
    match data with
    | [] -> total_return
    | stock :: rest ->
      print_stock_data stock; (* Print stock data to suppress warnings *)
      let signal = generate_signal stock factor in
      let profit = if signal = "BUY" then stock.returns else -.stock.returns in
      loop rest (total_return +. profit)
  in
  loop data 0.0

(* Example Data *)
let stock1 = { ticker = "AAPL"; date = "2025-03-28"; size = 1.2; value = 0.8; momentum = 1.5; returns = 0.02 }
let stock2 = { ticker = "GOOGL"; date = "2025-03-28"; size = 0.9; value = 1.1; momentum = 0.5; returns = -0.01 }

let factor_example = { size_premium = 0.02; value_premium = 0.03; momentum_premium = 0.05 }

let () =
  let data = [stock1; stock2] in
  let result = backtest data factor_example in
  Printf.printf "Total Portfolio Return: %.2f%%\n" (result *. 100.0)
