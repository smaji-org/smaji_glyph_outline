(*
 * smaji_glyph_outline.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

module Svg= Svg
module Utils= Utils

(*
type point= float * float

type segment=
  | Line of point
  | Qcurve of { ctrl: point; end': point }
  | Ccurve of { ctrl1: point; ctrl2:point; end': point }
  | SQcurve of point
  | SCcurve of { ctrl: point; end': point }
  | Arc of {
      rx: float;
      ry: float;
      large_arc: bool;
      sweep: bool;
      end': point;
    }

type path= {
  start: point;
  segments: segment list;
}
*)

