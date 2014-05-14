open Core

let () = Random.self_init ()

let width, height = 695, 478
let width', height' = Arrow.both float_of_int (width, height)

let main_container =
  Option.value_exn (Jq.jq "#content")

let () = Jq.css main_container [|"cursor", "none"|]

let random_poo () =
  let random_poo_type () = Random.int 9 in
  Draw.svg_file (Printf.sprintf "art/poo%d.svg" (random_poo_type ()))

let random_pos () = (Random.float width', Random.float height')

type rect = {top_left : float Draw.Point.t; width : float; height : float}

let covers ~top ~bottom =
  let (top_x, top_y) = top.top_left in
  let (bot_x, bot_y) = bottom.top_left in
  top_x <= bot_x &&
  bot_x +. bottom.width <= top_x +. top.width &&
  top_y <= bot_y &&
  bot_y +. bottom.height <= top_y +. top.height

let scrubbed hand_pos poo_rect =
  let hand_rect = let l = 70. in
    { top_left = Arrow.both (fun a -> a -. (l /. 2.)) hand_pos
    ; width = l
    ; height = l
    }
  in
  covers ~top:hand_rect ~bottom:poo_rect

let cursor_pos = Frp.latest (Jq.relative_mouse_pos main_container) ~init:(0, 0)
  |> Frp.Behavior.map ~f:(Arrow.both float_of_int)

let poos =
  let init = let open Draw in
    Array.init 100 ~f:(fun _ ->
      let top_left = random_pos () in
      ( transform (random_poo ()) (Frp.Behavior.return [|Transform.translate top_left|])
      , {top_left; width = 40.; height = 40.}))
  in let open Frp in print init;
  scan (Behavior.changes cursor_pos) ~init ~f:(fun poos hand_pos ->
    Array.filter poos ~f:(fun (_, poo_rect) -> not (scrubbed hand_pos poo_rect)))
  |> Frp.Behavior.map ~f:(fun poos -> Draw.pictures (Array.map ~f:fst poos))
  |> Draw.dynamic

let hand = let open Draw in
  let pos_b = Frp.Behavior.map cursor_pos ~f:(Arrow.both (fun a -> a -. 80.)) in
  transform (svg_file "art/hand.svg")
    (Frp.Behavior.map pos_b ~f:(fun pos -> [|Transform.translate pos|]))

let timer =
  let time = let open Frp in
    scan (Stream.ticks 1000.) ~init:20 ~f:(fun sec _ -> sec - 1)
    |> Behavior.map ~f:string_of_int
  in let open Frp.Behavior in
  Draw.text time (return (width' -. 10., 40.))
    ~props:[|
      return (Draw.Property.any ~name:"text-anchor" ~value:"end");
      return (Draw.Property.any ~name:"font-size" ~value:"30")
    |]

let background = Draw.svg_file "art/bathroom.svg"

let scene = Draw.pictures [|background; poos; hand; timer|]

let main () = begin
  let (svg, sub) = Draw.render_svg_node ~width ~height scene in
  Jq.Dom.append (Option.value_exn (Jq.to_dom_node main_container)) svg
end

