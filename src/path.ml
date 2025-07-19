(*
 * outline.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Utils

module Point = Point.PointF
type point = Point.t

type segment=
  | Line of point
  | Qcurve of { ctrl: point; end': point }
  | Ccurve of { ctrl1: point; ctrl2:point; end': point }
  | SQcurve of point
  | SCcurve of { ctrl: point; end': point }

type t= {
  start: point;
  segments: segment list;
}

type frame = {
  min_x: Point.cell; min_y: Point.cell;
  max_x: Point.cell; max_y: Point.cell;
}

let frame_dummy= {
  min_x= Float.infinity; min_y= Float.infinity;
  max_x= Float.neg_infinity; max_y= Float.neg_infinity;
}

let segment_to_string  ?(indent=0) segment=
  let open Printf in
  let indent= String.make indent ' ' in
  match segment with
  | Line point-> sprintf "%sLine %s" indent (Point.to_string point)
  | Qcurve { ctrl; end' }-> sprintf "%sQcurve { ctrl: %s; end: %s; }" indent (Point.to_string ctrl) (Point.to_string end')
  | Ccurve { ctrl1; ctrl2; end' }-> sprintf "%sCcurve { ctrl1: %s; ctrl2: %s; end: %s }" indent (Point.to_string ctrl1) (Point.to_string ctrl2)(Point.to_string end')
  | SQcurve point-> sprintf "%sSQcurve %s" indent (Point.to_string point)
  | SCcurve { ctrl; end' }-> sprintf "%sSCcurve { ctrl: %s; end: %s; }" indent (Point.to_string ctrl) (Point.to_string end')


let path_to_string ?(indent=0) path=
  let indent_str= String.make indent ' ' in
  let indent_str1= String.make (indent+2) ' ' in
  let start= Printf.sprintf "start: %s" (Point.to_string path.start) in
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

let frame_update (point:point) frame=
  let min_x= min point.x frame.min_x
  and min_y= min point.y frame.min_y
  and max_x= max point.x frame.max_x
  and max_y= max point.y frame.max_y in
  { min_x; min_y; max_x; max_y }

let frame_merge f1 f2=
  let min_x= min f1.min_x f2.min_x
  and min_y= min f1.min_y f2.min_y
  and max_x= max f1.max_x f2.max_x
  and max_y= max f1.max_y f2.max_y in
  { min_x; min_y; max_x; max_y }

let frame_to_string frame=
  Printf.sprintf "{ min_x= %s; min_y= %s; max_x= %s; max_y= %s }"
    (string_of_float frame.min_x)
    (string_of_float frame.min_y)
    (string_of_float frame.max_x)
    (string_of_float frame.max_y)

let frame_of_points points=
  List.fold_left (Fun.flip frame_update) frame_dummy points

let frame_segment prev segment=
  let init= frame_dummy |> frame_update prev in
  match segment with
  | Line end'-> init |> frame_update end'
  | Qcurve { ctrl; end' }->
    let plots= Bezier.plot_quadratic prev ctrl end' in
    frame_of_points plots
  | Ccurve { ctrl1; ctrl2; end' }->
    let plots= Bezier.plot_cubic prev ctrl1 ctrl2 end' in
    frame_of_points plots
  | SQcurve _-> invalid_arg "SQcurve"
  | SCcurve _-> invalid_arg "SCcurve"

let frame path=
  let rec calc acc prev_ctrl prev_end segments=
    match segments with
    | []-> (acc, prev_end)
    | Line end' ::tl->
      let acc= acc |> frame_update prev_end |> frame_update end' in
      calc acc None end' tl
    | Qcurve { ctrl; end' } ::tl->
      let acc= Bezier.plot_quadratic prev_end ctrl end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc (Some ctrl) end' tl
    | Ccurve { ctrl1; ctrl2; end' } ::tl->
      let acc= Bezier.plot_cubic prev_end ctrl1 ctrl2 end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc (Some ctrl2) end' tl
    | SQcurve end' ::tl->
      let ctrl=
        match prev_ctrl with
        | Some prev_ctrl-> Point.(prev_end + prev_end - prev_ctrl)
        | None-> prev_end
      in
      let acc= Bezier.plot_quadratic prev_end ctrl end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc (Some ctrl) end' tl
    | SCcurve { ctrl=ctrl2; end' } ::tl->
      let ctrl1=
        match prev_ctrl with
        | Some prev_ctrl-> Point.(prev_end + prev_end - prev_ctrl)
        | None-> prev_end
      in
      let acc= Bezier.plot_cubic prev_end ctrl1 ctrl2 end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc (Some ctrl2) end' tl
  in
  let acc= frame_dummy
  and prev_end= path.start
  and prev_ctrl= None in
  calc acc prev_ctrl prev_end path.segments

let frame_algo_svg path=
  let rec calc acc prev prev_ctrl prev_end segments=
    match segments with | []-> (acc, prev_end) | segment::tl->
    match segment with
    | Line end'->
      let acc= acc |> frame_update prev_end |> frame_update end' in
      calc acc segment None end' tl
    | Qcurve { ctrl; end' }->
      let acc= Bezier.plot_quadratic prev_end ctrl end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc segment (Some ctrl) end' tl
    | Ccurve { ctrl1; ctrl2; end' }->
      let acc= Bezier.plot_cubic prev_end ctrl1 ctrl2 end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc segment (Some ctrl2) end' tl
    | SQcurve end'->
      let ctrl=
        match prev_ctrl with
        | Some prev_ctrl->
          (match prev with
          | Qcurve _
          | SQcurve _ ->
            Point.(prev_end + prev_end - prev_ctrl)
          | _-> prev_end)
        | None-> prev_end
      in
      let acc= Bezier.plot_quadratic prev_end ctrl end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc segment (Some ctrl) end' tl
    | SCcurve { ctrl=ctrl2; end' }->
      let ctrl1=
        match prev_ctrl with
        | Some prev_ctrl->
          (match prev with
          | Ccurve _
          | SCcurve _ ->
            Point.(prev_end + prev_end - prev_ctrl)
          | _-> prev_end)
        | None-> prev_end
      in
      let acc= Bezier.plot_cubic prev_end ctrl1 ctrl2 end'
        |> frame_of_points
        |> frame_merge acc
      in
      calc acc segment (Some ctrl2) end' tl
  in
  let acc= frame_dummy
  and prev= Line {x= infinity; y= infinity}
  and prev_end= path.start
  and prev_ctrl= None in
  calc acc prev prev_ctrl prev_end path.segments

