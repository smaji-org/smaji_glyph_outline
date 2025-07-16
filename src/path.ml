(*
 * outline.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Point

type segment=
  | Line of PointF.t
  | Qcurve of { ctrl: PointF.t; end': PointF.t }
  | Ccurve of { ctrl1: PointF.t; ctrl2:PointF.t; end': PointF.t }
  | SQcurve of PointF.t
  | SCcurve of { ctrl: PointF.t; end': PointF.t }

type t= {
  start: PointF.t;
  segments: segment list;
}

type frame = {
  min_x: PointF.cell; min_y: PointF.cell;
  max_x: PointF.cell; max_y: PointF.cell;
}

let segment_to_string  ?(indent=0) segment=
  let open Printf in
  let indent= String.make indent ' ' in
  match segment with
  | Line point-> sprintf "%sLine %s" indent (PointF.to_string point)
  | Qcurve { ctrl; end' }-> sprintf "%sQcurve { ctrl: %s; end: %s; }" indent (PointF.to_string ctrl) (PointF.to_string end')
  | Ccurve { ctrl1; ctrl2; end' }-> sprintf "%sCcurve { ctrl1: %s; ctrl2: %s; end: %s }" indent (PointF.to_string ctrl1) (PointF.to_string ctrl2)(PointF.to_string end')
  | SQcurve point-> sprintf "%sSQcurve %s" indent (PointF.to_string point)
  | SCcurve { ctrl; end' }-> sprintf "%sSCcurve { ctrl: %s; end: %s; }" indent (PointF.to_string ctrl) (PointF.to_string end')


let path_to_string ?(indent=0) path=
  let indent_str= String.make indent ' ' in
  let indent_str1= String.make (indent+2) ' ' in
  let start= Printf.sprintf "start: %s" (PointF.to_string path.start) in
  let segements= path.segments
    |> List.map (segment_to_string ~indent:(indent+2))
    |> String.concat "\n"
  in
  Printf.sprintf "%s{\n%s%s\n%s\n%s}" indent_str indent_str1 start segements indent_str

let end_of_path path=
  match path.segments with
  | []-> None
  | _->
    Option.some @@ match path.segments |> List.rev |> List.hd with
    | Line point-> point
    | Qcurve { ctrl=_; end' }-> end'
    | Ccurve { ctrl1=_; ctrl2=_; end' }-> end'
    | SQcurve point-> point
    | SCcurve { ctrl=_; end' }-> end'

let is_closed path=
  match end_of_path path with
  | Some end'-> path.start = end'
  | None-> false

let is_open= Fun.negate is_closed

