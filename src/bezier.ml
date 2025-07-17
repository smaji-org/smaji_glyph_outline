(*
 * bezier.ml
 * -----------
 * Copyright : (c) 2025, smaji.org
 * Copyright : (c) 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)


module PointI = Point.PointI
module PointF = Point.PointF

type point = PointF.t

let lerp p0 p1 s=
  if s < 0. || s > 1. then invalid_arg "step is invalid";
  let open PointF in
  s *> p1 + (1. -. s) *> p0

let lerp2 p0 p1 p2 s=
  let p3= lerp p0 p1 s
  and p4= lerp p1 p2 s in
  lerp p3 p4 s

let lerp3 p0 p1 p2 p3 s=
  let p4= lerp p0 p1 s
  and p5= lerp p1 p2 s
  and p6= lerp p2 p3 s in
  let p7= lerp p4 p5 s
  and p8= lerp p5 p6 s in
  lerp p7 p8 s

let plot2 ?s p0 p1 p2=
  let s=
    match s with
    | Some s-> s
    | None-> [p0; p1; p2] |> PointF.perimeter |> Int.of_float
  in
  List.init (s+1)
    (fun step->
      lerp2 p0 p1 p2
      ((float_of_int step) /. (float_of_int s)))

let plot_quadratic= plot2

let plot3 ?s p0 p1 p2 p3=
  let s=
    match s with
    | Some s-> s
    | None-> [p0; p1; p2; p3] |> PointF.perimeter |> Int.of_float
  in  List.init (s+1)
    (fun step->
      lerp3 p0 p1 p2 p3
      ((float_of_int step) /. (float_of_int s)))

let plot_cubic= plot3

(*
let draw2 ?s p0 p1 p2=
  plot2 ?s p0 p1 p2
    |> List.map PointF.to_tuple

let draw_quadratic= draw2

let draw3 ?s p0 p1 p2 p3=
  plot3 ?s p0 p1 p2 p3
    |> List.map PointF.to_tuple

let draw_cubic= draw3
*)

(*
let best_fit plots= List.fold_left
  (fun ((p_min, p_max): point * point) (x,y)->
    let min_x= Float.min p_min.x x
    and min_y= Float.min p_min.y y
    and max_x= Float.max p_max.x x
    and max_y= Float.max p_max.y y in
    let min= PointF.{x= min_x; y= min_y}
    and max= PointF.{x= max_x; y= max_y} in
    (min, max)
  )
  PointF.(
    {x= Float.infinity; y= Float.infinity},
    {x= Float.neg_infinity; y= Float.neg_infinity}
  )
  plots
*)
