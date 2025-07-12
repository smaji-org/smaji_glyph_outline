(*
 * outline.mli
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

type point = float * float
(** The type of 2d point *)

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

val path_to_string : ?indent:int -> t -> string
(** Convert the path to printable string *)

val end_of_path : t -> point option
(** Return the endpoint of the path *)

val is_closed : t -> bool
(** Determine whether the path is closed i.e. an outline path *)

