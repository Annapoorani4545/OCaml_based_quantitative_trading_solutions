(* Define order types *)
type order_type = Buy | Sell

type order = {
  id : int;
  order_type : order_type;
  price : float;
  quantity : int;
}

(* Order Book using a priority queue *)
module OrderBook = struct
  module OrderCompare = struct
    type t = order
    let compare a b = match a.order_type with
      | Buy -> compare b.price a.price (* Higher price has priority for Buy *)
      | Sell -> compare a.price b.price (* Lower price has priority for Sell *)
  end
  module OrderPQ = Set.Make(OrderCompare)

  type t = {
    mutable buy_orders : OrderPQ.t;
    mutable sell_orders : OrderPQ.t;
  }

  let create () = { buy_orders = OrderPQ.empty; sell_orders = OrderPQ.empty }

  let add_order book order =
    match order.order_type with
    | Buy -> book.buy_orders <- OrderPQ.add order book.buy_orders
    | Sell -> book.sell_orders <- OrderPQ.add order book.sell_orders

  let remove_order book order_id =
    let filter_fn o = o.id <> order_id in
    book.buy_orders <- OrderPQ.filter filter_fn book.buy_orders;
    book.sell_orders <- OrderPQ.filter filter_fn book.sell_orders

  let match_orders book =
    let rec match_loop () =
      match OrderPQ.min_elt_opt book.buy_orders, OrderPQ.min_elt_opt book.sell_orders with
      | Some buy_order, Some sell_order when buy_order.price >= sell_order.price ->
        let trade_qty = min buy_order.quantity sell_order.quantity in
        Printf.printf "Trade executed: %d @ %.2f\n" trade_qty sell_order.price;
        if buy_order.quantity > sell_order.quantity then
          book.buy_orders <- OrderPQ.add { buy_order with quantity = buy_order.quantity - trade_qty } (OrderPQ.remove buy_order book.buy_orders)
        else
          book.buy_orders <- OrderPQ.remove buy_order book.buy_orders;
        if sell_order.quantity > buy_order.quantity then
          book.sell_orders <- OrderPQ.add { sell_order with quantity = sell_order.quantity - trade_qty } (OrderPQ.remove sell_order book.sell_orders)
        else
          book.sell_orders <- OrderPQ.remove sell_order book.sell_orders;
        match_loop ()
      | _ -> ()
    in
    match_loop ()
end

(* Example usage *)
let () =
  let book = OrderBook.create () in
  OrderBook.add_order book { id = 1; order_type = Buy; price = 100.0; quantity = 50 };
  OrderBook.add_order book { id = 2; order_type = Sell; price = 99.0; quantity = 30 };
  OrderBook.add_order book { id = 3; order_type = Sell; price = 101.0; quantity = 70 };

  Printf.printf "\nRemoving order with ID 2...\n";
  OrderBook.remove_order book 2;

  Printf.printf "\nMatching orders...\n";
  OrderBook.match_orders book;
