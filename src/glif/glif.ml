(*
 * glif.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Utils

type point = float * float

type cubic_desc = { ctrl1 : point; ctrl2 : point; end' : point; }

type quadratic_desc = { ctrl : point; end' : point; }

let xml_member name nodes=
  try
    Some (Ezxmlm.member name nodes)
  with
    Ezxmlm.Tag_not_found _-> None

type component= {
  base: string option;
  xScale: float;
  xyScale: float;
  yxScale: float;
  yScale: float;
  xOffset: float;
  yOffset: float;
  identifier: string option;
}

module Raw = struct
  type contour_point_type=
    | Line
    | Offcurve
    | Curve
    | Qcurve

  let contour_point_type_of_string= function
    | "line"     -> Line
    | "offcurve" -> Offcurve
    | "curve"    -> Curve
    | "qcurve"   -> Qcurve
    | _          -> Offcurve

  let contour_point_type_to_string= function
    | Line     -> "line"
    | Offcurve -> "offcurve"
    | Curve    -> "curve"
    | Qcurve   -> "qcurve"

  type contour_point= {
    x: float;
    y: float;
    point_type: contour_point_type;
  }

  type contour= {
    identifier: string option;
    points: contour_point list;
  }

  let contour_point_to_string point=
    Printf.sprintf "%s (%s,%s)"
      (contour_point_type_to_string point. point_type)
      (Utils.string_of_float point.x)
      (Utils.string_of_float point.y)

  let component_default= {
    base= None;
    xScale= 1.;
    xyScale= 0.;
    yxScale= 0.;
    yScale= 1.;
    xOffset= 0.;
    yOffset= 0.;
    identifier= None;
  }

  let contour_point_default= {
    x= 0.;
    y= 0.;
    point_type= Offcurve;
  }

  let contour_default= {
    identifier= None;
    points= [];
  }

  type outline_elm=
    | Component of component
    | Contour of contour

  type glif= {
    name: string;
    format: string;
    formatMinor: string;
    elements: outline_elm list;
  }

  let get_component attrs=
    ListLabels.fold_left
      attrs
      ~init:component_default
      ~f:(fun acc attr->
        let ((_ns, name), value)= attr in
        match name with
        | "base"-> { acc with base= Some value }
        | "xScale"-> { acc with xScale= float_of_string value }
        | "xyScale"-> { acc with xyScale= float_of_string value }
        | "yxScale"-> { acc with yxScale= float_of_string value }
        | "yScale"-> { acc with yScale= float_of_string value }
        | "xOffset"-> { acc with xOffset= float_of_string value }
        | "yOffset"-> { acc with yOffset= float_of_string value }
        | "identifier"-> { acc with identifier= Some value }
        | _-> acc)

  let get_point attrs=
    ListLabels.fold_left
      attrs
      ~init:component_default
      ~f:(fun acc attr->
        let ((_ns, name), value)= attr in
        match name with
        | "x"-> { acc with base= Some value }
        | "y"-> { acc with xScale= float_of_string value }
        | "type"-> { acc with xyScale= float_of_string value }
        | _-> acc)

  let _load_file path=
    In_channel.with_open_text path @@ fun chan->
    let _dtd, nodes= Ezxmlm.from_channel chan in
    let attrs, glyph= Ezxmlm.member_with_attr "glyph" nodes in
    let name= attrs |> Ezxmlm.get_attr "name"
    and format= attrs |> Ezxmlm.get_attr "format"
    and formatMinor= attrs
      |> xml_get_attr_opt "formatMinor"
      |> Option.value ~default:"0"
    and _, outline= Ezxmlm.member_with_attr "outline" glyph in
    let elements= ListLabels.filter_map
      outline
      ~f:(fun node->
        match node with
        | `El (((_ns,name), attrs), nodes)->
          (match name with
          | "component"-> Some (Component (get_component attrs))
          | "contour"->
            let identifier= xml_get_attr_opt "identifier" attrs in
            let points= ListLabels.filter_map nodes
              ~f:(fun node->
                match node with
                | `El (((_ns, "point"), attrs), _)->
                  let x= attrs
                    |> xml_get_attr_opt "x"
                    |> Option.value ~default:"0.0"
                    |> float_of_string in
                  let y= attrs
                    |> xml_get_attr_opt "y"
                    |> Option.value ~default:"0.0"
                    |> float_of_string in
                  let point_type= attrs
                    |> xml_get_attr_opt "type"
                    |> Option.value ~default:""
                    |> contour_point_type_of_string
                  in
                  Some {
                    x;
                    y;
                    point_type;
                  }
                | _-> None
                )
            in
            Some (Contour {
              identifier;
              points;
            })
          | _-> None)
        | `Data _-> None)
    in
    {
      name;
      format;
      formatMinor;
      elements;
    }

  let load_file_exn path=
    try _load_file path with _-> failwith "load_file"

  let load_file path=
    try Some (_load_file path) with _-> None
end

type ('a, 'b) either=
  | Left of 'a
  | Right of 'b

let outline_of_points (points:Raw.contour_point list)=
  let rec find_start elt=
    match elt.Circle.value.Raw.point_type with
    | Line-> elt
    | Offcurve-> find_start elt.right
    | Curve-> elt
    | Qcurve-> elt
  in
  let circle= Circle.of_list points in
  let build_next building (elt:Raw.contour_point Circle.elt)=
    let value= elt.value in
    match value.point_type with
    | Line-> Right (Outline.Line (value.x, value.y))
    | Offcurve-> Left (Dlist.insert_last building value)
    | Curve->
      (match Dlist.length building with
      | 0-> Right (Outline.Line (value.x, value.y))
      | 1->
        let elt_ctrl= Dlist.head building |> Option.get in
        let ctrl= (elt_ctrl.value.x, elt_ctrl.value.y) in
        let end'= (elt.value.x, elt.value.y) in
        Right (Outline.Qcurve { ctrl ; end' })
      | 2->
        let elt_ctrl1= Dlist.head building |> Option.get in
        let elt_ctrl2= elt_ctrl1.right |> Option.get in
        let ctrl1= (elt_ctrl1.value.x, elt_ctrl1.value.y) in
        let ctrl2= (elt_ctrl2.value.x, elt_ctrl2.value.y) in
        let end'= (elt.value.x, elt.value.y) in
        Right (Outline.Ccurve { ctrl1; ctrl2; end' })
      | _-> assert false;
      )
    | Qcurve->
      (match Dlist.length building with
      | 0-> Right (Outline.Line (value.x, value.y))
      | 1->
        let elt_ctrl= Dlist.head building |> Option.get in
        let ctrl= (elt_ctrl.value.x, elt_ctrl.value.y) in
        let end'= (elt.value.x, elt.value.y) in
        Right (Outline.Qcurve { ctrl ; end' })
      | _-> assert false;
      )
  in
  match Circle.entry circle with
  | None-> None
  | Some entry->
    let start= find_start entry in
    let[@tail_mod_cons] rec build building point=
      match build_next building point with
      | Left building-> build building point.right
      | Right segment->
        if point == start then
          [segment]
        else
          let building= Dlist.of_list [] in
          segment :: build building point.right
    in
    let building= Dlist.of_list [] in
    let segments= build building start.right in
    let start= (start.value.x, start.value.y) in
    Some Outline.{ start; segments }

let outline_to_points (path:Outline.path)=
  let dummy= ((0.,0.),(0.,0.)) in
  let rec to_points prev(*used to calc the reflection of the control point*) (segments:Outline.segment list)=
    match segments with
    | []-> []
    | segment::tl->
      match segment with
      | Line (x, y)-> Raw.{ x; y; point_type= Line } :: to_points dummy tl
      | Qcurve { ctrl; end'; }->
        let p1=
          let (x,y)= ctrl in
          Raw.{ x; y; point_type= Offcurve }
        and p2=
          let (x,y)= end' in
          Raw.{ x; y; point_type= Qcurve } in
        p1::p2 :: to_points (ctrl, end') tl
      | Ccurve { ctrl1; ctrl2; end'; }->
        let p1=
          let (x,y)= ctrl1 in
          Raw.{ x; y; point_type= Offcurve }
        and p2=
          let (x,y)= ctrl2 in
          Raw.{ x; y; point_type= Offcurve }
        and p3=
          let (x,y)= end' in
          Raw.{ x; y; point_type= Curve } in
        p1::p2::p3 :: to_points (ctrl2, end') tl
      | SQcurve end'->
        let ctrl=
          let ((x1,y1), (x2,y2))= prev in
          (x2 *. 2. -. x1, y2 *. 2. -. y1)
        in
        let p1=
          let (x,y)= ctrl in
          Raw.{ x; y; point_type= Offcurve }
        and p2=
          let (x,y)= end' in
          Raw.{ x; y; point_type= Qcurve } in
        p1::p2 :: to_points (ctrl, end') tl
      | SCcurve { ctrl; end'; } ->
        let ctrl1=
          let ((x1,y1), (x2,y2))= prev in
          (x2 *. 2. -. x1, y2 *. 2. -. y1)
        in
        let p1=
          let (x,y)= ctrl1 in
          Raw.{ x; y; point_type= Offcurve }
        and p2=
          let (x,y)= ctrl in
          Raw.{ x; y; point_type= Offcurve }
        and p3=
          let (x,y)= end' in
          Raw.{ x; y; point_type= Curve } in
        p1::p2::p3 :: to_points (ctrl, end') tl
  in
  to_points dummy path.segments

