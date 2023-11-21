(*
 * smaji_glyph_outline.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

module Svg = Svg
(** Module used for svg supporting *)

module Utils = Utils
(** Internal utils *)

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

type path = { start : point; segments : segment list; }
(** The type of glyph path *)

