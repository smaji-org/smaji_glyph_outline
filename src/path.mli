(*
 * outline.mli
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

module Point = Point.PointF
type point= Point.t

(** The type of 2d segment *)
type segment =
  | Line of point
    (** Line segment from previous point to point *)
  | Qcurve of { ctrl : point; end' : point; }
    (** Quadratic Bézier Curve consists of previous point, ctrl and end' *)
  | Ccurve of { ctrl1 : point; ctrl2 : point; end' : point; }
    (** Cubic Bézier Curve consists of previous point, ctrl1, ctrl2 and end' *)
  | SQcurve of point
    (** Quadratic Bézier Curve consists of previous end', previous reflection of ctrl and point *)
  | SCcurve of { ctrl : point; end' : point; }
    (** Cubic Bézier Curve consists of previous end', previous reflection of ctrl2, ctrl and end' *)


type t = { start : point; segments : segment list; }
(** The type of glyph path *)

type frame = {
  min_x: Point.cell; min_y: Point.cell;
  max_x: Point.cell; max_y: Point.cell;
}

val frame_merge : frame -> frame -> frame
val frame_update : point -> frame -> frame
val frame_to_string : frame -> string

val path_to_string : ?indent:int -> t -> string
(** Convert the path to printable string *)

val end_of_path : t -> point option
(** Return the endpoint of the path *)

val is_closed : t -> bool
(** Determine whether the path is closed i.e. an outline path *)

val is_open : t -> bool
(** Determine whether the path is open i.e. a stroke path *)

val frame_segment : point -> segment -> frame
(** calcuate the best fit frame from the given start point and segment *)

val frame : t -> frame * point
(** calcuate the best fit frame of the path *)

val frame_algo_svg : t -> frame * point
(** calcuate the best fit frame of the path by the algorithm used in SVG image processing *)
