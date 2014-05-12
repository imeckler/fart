open Core

let () = Random.self_init ()

let arrow radius = let open Frp.Behavior in let open Draw in
  circle (return radius) (return (20., 20.))
    ~props:[|return (Property.fill (Color.random ()))|]

let mom_text font_size = let open Draw in let open Frp.Behavior in
  let msg = [|"Happy"; "Mother's"; "Day!"|].(Random.int 3) in
  text (return msg) (return (20., 20.)) 
    ~props:[|return (Property.any ~name:"font-size" ~value:(string_of_int font_size))|]
(*
  Draw.image ~width:(return 200) ~height:(return 60)
    (return "art/arrow.svg") *)

let width, height = 1000, 1000
let width', height' = float_of_int width, float_of_int height

let flying_arrow () = let open Draw in
  let c = 0.5 +. Random.float 1. in
  (*
  let color =
    let c = int_of_float (255. *. (speed /. 1.5)) 
    in Color.of_rgb ~r:c ~g:c ~b:c () in *)
  let radius = (7. *. (c /. 1.5)) ** 2. in
  let speed = c *. 0.5 in
  let font_size = int_of_float (c *. 20.) in
  let y_pos = float_of_int (Random.int height) in
  let trans_x = Frp.(scan (Stream.deltas 30.) ~init:0. ~f:(fun x t ->
    x +. Time.Span.to_ms t *. speed
  ))
  in
  let composite = pictures [|arrow radius; mom_text font_size;|] in
  ( transform composite (Frp.Behavior.map trans_x ~f:(fun x ->
    [| Transform.Translate (x, y_pos) |]))
  , trans_x
  )

let arrows =
  let spaces = Frp.Stream.filter (Frp.Behavior.changes Jq.keys)
    ~f:(Array.exists ~f:(fun k -> Jq.Event.Key.to_code k = 32))
  in
  Frp.scan spaces ~init:[||] ~f:(fun arrs _ ->
    Array.filter_map arrs ~f:(fun (a, xb) ->
      (* A bit broken since the depencencies aren't properly expressed.
       * The array is only cleared out when the space button is pressed *)
      if Frp.Behavior.peek xb <= width' then Some (a, xb) else None)
    |> Array.append [| flying_arrow () |]
  )
  |> Frp.Behavior.map ~f:(fun arrs ->
      Draw.pictures (Array.map ~f:fst arrs))
  |> Draw.dynamic

let (svg, sub) = Draw.render_svg_node ~width ~height arrows

let main () = Jq.Dom.append (Option.(value_exn (bind (Jq.jq "#content") Jq.to_dom_node))) svg

