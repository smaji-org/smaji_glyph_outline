(*
 * bezier.mli
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

val lerp : PointF.t -> PointF.t -> PointF.cell -> PointF.t

val lerp2 :
  PointF.t -> PointF.t -> PointF.t -> PointF.cell -> PointF.t

val lerp3 :
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.cell ->
  PointF.t

val plot2 :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t list

val plot_quadratic :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t list

val plot3 :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t list

val plot_cubic :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t list

(*
val draw2 :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  (PointF.cell * PointF.cell) list

val draw_quadratic :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  (PointF.cell * PointF.cell) list

val draw3 :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  (PointF.cell * PointF.cell) list

val draw_cubic :
  ?s:int ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  PointF.t ->
  (PointF.cell * PointF.cell) list
*)

(* val best_fit : (Float.t * Float.t) array -> point * point *)
