(* OCaml Monte Carlo Simulation for Option Pricing *)

(* Helper functions *)
let pi = 4.0 *. atan 1.0

let random_normal () =
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. pi *. u2)

(* Simulating Geometric Brownian Motion *)
let geometric_brownian_motion s0 mu sigma t dt =
  let steps = int_of_float (t /. dt) in
  let rec simulate s path n =
    if n = 0 then List.rev path
    else
      let wt = sqrt dt *. random_normal () in
      let st = s *. exp ((mu -. 0.5 *. sigma ** 2.0) *. dt +. sigma *. wt) in
      simulate st (st :: path) (n - 1)
  in
  simulate s0 [s0] steps

(* Monte Carlo Simulation for Option Pricing *)
let monte_carlo_option_pricing s0 k t r sigma n call_option =
  let dt = 0.01 in
  let payoff st = if call_option then max 0.0 (st -. k) else max 0.0 (k -. st) in
  let rec simulate_sum i sum =
    if i = 0 then sum
    else
      let path = geometric_brownian_motion s0 r sigma t dt in
      let st = List.hd (List.rev path) in
      simulate_sum (i - 1) (sum +. payoff st)
  in
  let discounted_sum = simulate_sum n 0.0 /. float n in
  exp (-.r *. t) *. discounted_sum

(* Example Usage *)
let () =
  let s0 = 100.0 in    (* Initial stock price *)
  let k = 100.0 in     (* Strike price *)
  let t = 1.0 in       (* Time to maturity (years) *)
  let r = 0.05 in      (* Risk-free rate *)
  let sigma = 0.2 in   (* Volatility *)
  let n = 10000 in     (* Number of simulations *)

  let call_price = monte_carlo_option_pricing s0 k t r sigma n true in
  let put_price = monte_carlo_option_pricing s0 k t r sigma n false in

  Printf.printf "Call Option Price: %.4f\n" call_price;
  Printf.printf "Put Option Price: %.4f\n" put_price;
