let width, height = 695, 478

let main_container =
  Option.value_exn (Jq.jq "#content")

let hand = let open Draw in
  let pos_b = Frp.latest (Jq.relative_mouse_pos main_container) ~init:(0, 0)
    |> Frp.Behavior.map ~f:(Arrow.both float_of_int)
  in
  transform
    (svg_file "art/hand.svg")
    (Frp.Behavior.map pos_b ~f:(fun pos -> [|Transform.translate pos|]))

let background = Draw.svg_file "art/bathroom.svg"

let scene = Draw.pictures [|hand; background|]

let () = begin
  let (svg, sub) = Draw.render_svg_node ~width ~height scene in
  Jq.Dom.append (Option.value_exn (Jq.to_dom_node main_container)) svg
end

