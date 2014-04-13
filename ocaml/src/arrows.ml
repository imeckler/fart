open Core

let () = Random.self_init ()

let (arrow : Draw.t) = failwith ""

let width, height = 1000, 1000
let width', height' = float_of_int width, float_of_int height

let flying_arrow () =
  let speed = 1. in
  let y_pos = float_of_int (Random.int height) in
  let trans_x = Frp.(scan (Stream.deltas 30.) ~init:0. ~f:(fun x t ->
    x +. Time.Span.to_ms t *. speed
  ))
  in let open Draw in
  transform arrow (Frp.Behavior.map trans_x ~f:(fun x ->
    [| Transform.Translate (x, y_pos) |]))

let arrows =
  let spaces = Frp.Stream.filter (Frp.Behavior.changes Jq.keys)
    ~f:(Array.exists ~f:(fun k -> Jq.Event.Key.to_code k = 32))
  in
  Frp.scan spaces ~init:[||] ~f:(fun arrs _ ->
    Array.append [|flying_arrow ()|] arrs
  )
  |> Frp.Behavior.map ~f:Draw.pictures
  |> Draw.dynamic

let (svg, sub) = Draw.render_svg_node ~width ~height arrows

let main () = Jq.Dom.append (Option.(value_exn (bind (Jq.jq "#content") Jq.to_dom_node))) svg

