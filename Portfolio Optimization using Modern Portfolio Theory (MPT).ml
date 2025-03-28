(* Portfolio Optimization using Markowitz's Modern Portfolio Theory *)

(* Helper Functions *)
let mean lst =
  let sum = List.fold_left ( +. ) 0.0 lst in
  sum /. float_of_int (List.length lst)

let variance lst =
  let mean_val = mean lst in
  let squared_diffs = List.map (fun x -> (x -. mean_val) ** 2.0) lst in
  mean squared_diffs

let covariance lst1 lst2 =
  let mean1 = mean lst1 in
  let mean2 = mean lst2 in
  let products = List.map2 (fun x y -> (x -. mean1) *. (y -. mean2)) lst1 lst2 in
  mean products

(* Calculate Portfolio Return *)
let portfolio_return weights returns =
  List.fold_left2 (fun acc w r -> acc +. (w *. r)) 0.0 weights returns

(* Calculate Portfolio Variance *)
let portfolio_variance weights returns =
  let n = List.length weights in
  let variances = ref 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let cov = if i = j then variance (List.nth returns i) else covariance (List.nth returns i) (List.nth returns j) in
      variances := !variances +. (List.nth weights i *. List.nth weights j *. cov)
    done
  done;
  !variances

(* Sharpe Ratio Calculation *)
let sharpe_ratio portfolio_return portfolio_std risk_free_rate =
  (portfolio_return -. risk_free_rate) /. portfolio_std

(* Gradient Descent for Optimization *)
let rec optimize weights returns risk_free_rate learning_rate epochs =
  if epochs = 0 then weights else
    let ret = portfolio_return weights (List.map mean returns) in
    let var = portfolio_variance weights returns in
    let std_dev = sqrt var in

    (* Calculate and Print Sharpe Ratio *)
    let sharpe = sharpe_ratio ret std_dev risk_free_rate in
    Printf.printf "Sharpe Ratio: %f\n" sharpe;

    let gradients = List.map (fun _ -> (ret -. risk_free_rate) /. (std_dev *. sqrt var)) weights in

    let new_weights = List.map2 (fun w g -> w +. learning_rate *. g) weights gradients in
    let sum_weights = List.fold_left ( +. ) 0.0 new_weights in
    let normalized_weights = List.map (fun w -> w /. sum_weights) new_weights in

    optimize normalized_weights returns risk_free_rate learning_rate (epochs - 1)

(* Example Usage *)
let () =
  let returns = [ [0.1; 0.2; 0.15]; [0.05; 0.1; 0.08]; [0.12; 0.18; 0.14] ] in
  let initial_weights = [0.33; 0.33; 0.34] in
  let risk_free_rate = 0.03 in
  let learning_rate = 0.01 in
  let epochs = 1000 in

  let optimized_weights = optimize initial_weights returns risk_free_rate learning_rate epochs in
  Printf.printf "Optimized Portfolio Weights: [ %f; %f; %f ]\n" (List.nth optimized_weights 0) (List.nth optimized_weights 1) (List.nth optimized_weights 2)
