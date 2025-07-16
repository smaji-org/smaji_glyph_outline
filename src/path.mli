(*
 * outline.mli
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Point

(** The type of 2d segment *)
type segment =
  | Line of PointF.t
    (** Line segment from previous point to point *)
  | Qcurve of { ctrl : PointF.t; end' : PointF.t; }
    (** Quadratic Bézier Curve consists of previous point, ctrl and end' *)
  | Ccurve of { ctrl1 : PointF.t; ctrl2 : PointF.t; end' : PointF.t; }
    (** Cubic Bézier Curve consists of previous point, ctrl1, ctrl2 and end' *)
  | SQcurve of PointF.t
    (** Quadratic Bézier Curve consists of previous end', previous reflection of ctrl and point *)
  | SCcurve of { ctrl : PointF.t; end' : PointF.t; }
    (** Cubic Bézier Curve consists of previous end', previous reflection of ctrl2, ctrl and end' *)


type t = { start : PointF.t; segments : segment list; }
(** The type of glyph path *)

type frame = {
  min_x: PointF.cell; min_y: PointF.cell;
  max_x: PointF.cell; max_y: PointF.cell;
}

val path_to_string : ?indent:int -> t -> string
(** Convert the path to printable string *)

val end_of_path : t -> PointF.t option
(** Return the endpoint of the path *)

val is_closed : t -> bool
(** Determine whether the path is closed i.e. an outline path *)

val is_open : t -> bool
(** Determine whether the path is open i.e. a stroke path *)

