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

