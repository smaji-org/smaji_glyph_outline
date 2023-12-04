(*
 * outline.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

type point= float * float

type segment=
  | Line of point
  | Qcurve of { ctrl: point; end': point }
  | Ccurve of { ctrl1: point; ctrl2:point; end': point }
  | SQcurve of point
  | SCcurve of { ctrl: point; end': point }

type path= {
  start: point;
  segments: segment list;
}

let point_to_string (p1,p2)= Printf.sprintf "(%s,%s)"
  (Utils.string_of_float p1)
  (Utils.string_of_float p2)

let segment_to_string  ?(indent=0) segment=
  let open Printf in
  let indent= String.make indent ' ' in
  match segment with
  | Line point-> sprintf "%sLine %s" indent (point_to_string point)
  | Qcurve { ctrl; end' }-> sprintf "%sQcurve { ctrl: %s; end: %s; }" indent (point_to_string ctrl) (point_to_string end')
  | Ccurve { ctrl1; ctrl2; end' }-> sprintf "%sCcurve { ctrl1: %s; ctrl2: %s; end: %s }" indent (point_to_string ctrl1) (point_to_string ctrl2)(point_to_string end')
  | SQcurve point-> sprintf "%sSQcurve %s" indent (point_to_string point)
  | SCcurve { ctrl; end' }-> sprintf "%sSCcurve { ctrl: %s; end: %s; }" indent (point_to_string ctrl) (point_to_string end')


let path_to_string ?(indent=0) path=
  let indent_str= String.make indent ' ' in
  let indent_str1= String.make (indent+2) ' ' in
  let start= Printf.sprintf "start: %s" (point_to_string path.start) in
  let segements= path.segments
    |> List.map (segment_to_string ~indent:(indent+2))
    |> String.concat "\n"
  in
  Printf.sprintf "%s{\n%s%s\n%s\n%s}" indent_str indent_str1 start segements indent_str

