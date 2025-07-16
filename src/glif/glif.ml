(*
 * glif.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Utils
open Printf

module Point= Point.PointF
type point = Point.t

type cubic_desc = { ctrl1 : point; ctrl2 : point; end' : point; }

type quadratic_desc = { ctrl : point; end' : point; }

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

let string_of_contour_point_type= function
  | Line     -> "line"
  | Offcurve -> "offcurve"
  | Curve    -> "curve"
  | Qcurve   -> "qcurve"

type contour_point= {
  p: point;
  typ: contour_point_type;
}

type contour= {
  identifier: string option;
  points: contour_point list;
}

let string_of_contour_point point=
  sprintf "%s (%s,%s)"
    (string_of_contour_point_type point.typ)
    (Utils.string_of_float point.p.x)
    (Utils.string_of_float point.p.y)

let glif_string_of_contour_point ?(indent=0) point=
  let indent_str= String.make indent ' ' in
  sprintf "%s<point x=\"%s\" y=\"%s\" type=\"%s\" />"
    indent_str
    (string_of_float point.p.x)
    (string_of_float point.p.y)
    (string_of_contour_point_type point.typ)

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

(*
let contour_point_default= {
  x= 0.;
  y= 0.;
  point_type= Offcurve;
}

let contour_default= {
  identifier= None;
  points= [];
}
*)

type outline_elm=
  | Component of component
  | Contour of contour

type advance= { width: float; height: float }

type unicodes= int list

type t= {
  name: string;
  format: int;
  formatMinor: int;
  advance: advance;
  unicodes: unicodes;
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

(*
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
*)

let get_outline glyph=
  let _, outline= Ezxmlm.member_with_attr "outline" glyph in
  ListLabels.filter_map
    outline
    ~f:(fun node->
      match node with
      | `El (((_ns,name), attrs), nodes)->
        (match name with
        | "component"-> Some (Component (get_component attrs))
        | "contour"->
          let identifier= xml_attr_opt "identifier" attrs in
          let points= ListLabels.filter_map nodes
            ~f:(fun node->
              match node with
              | `El (((_ns, "point"), attrs), _)->
                let x= attrs
                  |> xml_attr_opt "x"
                  |> Option.value ~default:"0.0"
                  |> float_of_string in
                let y= attrs
                  |> xml_attr_opt "y"
                  |> Option.value ~default:"0.0"
                  |> float_of_string in
                let p= Point.{x;y} in
                let typ= attrs
                  |> xml_attr_opt "type"
                  |> Option.value ~default:""
                  |> contour_point_type_of_string
                in
                Some {
                  p;
                  typ;
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

let get_advance glyph=
  match Ezxmlm.member_with_attr "advance" glyph with
  | attrs, _->
    let width= xml_attr_opt "width" attrs
      |> Fun.flip Option.bind float_of_string_opt
    and height= xml_attr_opt "width" attrs
      |> Fun.flip Option.bind float_of_string_opt in
    (match width, height with
    | None, None-> { height= 0.; width= 0. }
    | Some width, None-> { width= 0.; height= width }
    | None, Some height-> { width= height; height }
    | Some width, Some height-> { width; height }
    )
  | exception Not_found-> { height= 0.; width= 0. }

let get_unicode glyph= glyph
  |> Ezxmlm.members_with_attr "unicode"
  |> List.filter_map (fun (attrs,_)->
    attrs |> xml_attr_opt "hex" |> Option.map int_of_hex)

let of_xml_nodes nodes=
  let attrs, glyph= Ezxmlm.member_with_attr "glyph" nodes in
  let name= attrs |> Ezxmlm.get_attr "name"
  and format= attrs
    |> xml_attr_opt "format"
    |> Option.map int_of_string
    |> Option.value ~default:2
  and formatMinor= attrs
    |> xml_attr_opt "formatMinor"
    |> Option.map int_of_string
    |> Option.value ~default:0
  and advance= get_advance glyph
  and unicodes= get_unicode glyph
  and elements= get_outline glyph in
  {
    name;
    format;
    formatMinor;
    advance;
    unicodes;
    elements;
  }


let of_string string=
  let _dtd, nodes= Ezxmlm.from_string string in
  of_xml_nodes nodes

let _load_file path=
  In_channel.with_open_text path @@ fun chan->
  let _dtd, nodes= Ezxmlm.from_channel chan in
  of_xml_nodes nodes

let load_file_exn path=
  try _load_file path with _-> failwith "load_file"

let load_file path=
  try Some (_load_file path) with _-> None

type ('a, 'b) either=
  | Left of 'a
  | Right of 'b

let outline_of_points (points:contour_point list)=
  let rec find_start elt=
    match elt.Circle.value.typ with
    | Line-> elt
    | Offcurve-> find_start elt.right
    | Curve-> elt
    | Qcurve-> elt
  in
  let circle= Circle.of_list points in
  let build_next building (elt:contour_point Circle.elt)=
    let open Point in
    let value= elt.value in
    match value.typ with
    | Line-> Right (Path.Line {x= value.p.x; y= value.p.y})
    | Offcurve-> Left (Dlist.insert_last building value)
    | Curve->
      (match Dlist.length building with
      | 0-> Right (Path.Line {x= value.p.x; y= value.p.y})
      | 1->
        let elt_ctrl= Dlist.head building |> Option.get in
        let ctrl= Point.{x= elt_ctrl.value.p.x; y= elt_ctrl.value.p.y} in
        let end'= Point.{x= elt.value.p.x; y= elt.value.p.y} in
        Right (Path.Qcurve { ctrl ; end' })
      | 2->
        let elt_ctrl1= Dlist.head building |> Option.get in
        let elt_ctrl2= elt_ctrl1.right |> Option.get in
        let ctrl1= {x=elt_ctrl1.value.p.x; y= elt_ctrl1.value.p.y} in
        let ctrl2= {x=elt_ctrl2.value.p.x; y=elt_ctrl2.value.p.y} in
        let end'= {x=elt.value.p.x; y=elt.value.p.y} in
        Right (Path.Ccurve { ctrl1; ctrl2; end' })
      | _-> assert false;
      )
    | Qcurve->
      (match Dlist.length building with
      | 0-> Right (Path.Line {x=value.p.x; y=value.p.y})
      | 1->
        let elt_ctrl= Dlist.head building |> Option.get in
        let ctrl= elt_ctrl.value.p in
        let end'= elt.value.p in
        Right (Path.Qcurve { ctrl ; end' })
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
    let start= start.value.p in
    Some Path.{ start; segments }

let points_of_path (path:Path.t)=
  let dummy= (Point.zero, Point.zero) in
  let rec to_points
    prev (* used to calc the reflection of the control point *)
    (segments: Path.segment list)
    =
    match segments with
    | []-> []
    | segment::tl->
      match segment with
      | Line p-> { p; typ= Line } :: to_points dummy tl
      | Qcurve { ctrl; end'; }->
        let p1=
          { p= ctrl; typ= Offcurve }
        and p2=
          { p= end'; typ= Qcurve } in
        p1::p2 :: to_points (ctrl, end') tl
      | Ccurve { ctrl1; ctrl2; end'; }->
        let p1=
          { p= ctrl1; typ= Offcurve }
        and p2=
          { p= ctrl2; typ= Offcurve }
        and p3=
          { p= end'; typ= Curve } in
        p1::p2::p3 :: to_points (ctrl2, end') tl
      | SQcurve end'->
        let ctrl=
          let (p1, p2)= prev in
          Point.(p2 + p2 - p1)
        in
        let p1=
          { p= ctrl; typ= Offcurve }
        and p2=
          { p= end'; typ= Qcurve } in
        p1::p2 :: to_points (ctrl, end') tl
      | SCcurve { ctrl; end'; } ->
        let ctrl1=
          let (p1, p2)= prev in
          Point.(p2 + p2 - p1)
        in
        let p1=
          { p= ctrl1; typ= Offcurve }
        and p2=
          { p= ctrl; typ= Offcurve }
        and p3=
          { p= end'; typ= Curve } in
        p1::p2::p3 :: to_points (ctrl, end') tl
  in
  to_points dummy path.segments

let points_of_outline (path:Path.t)=
  if Path.is_open path then
    None
  else
    path |> points_of_path |> Option.some

let points_of_outline_exn (path:Path.t)=
  if Path.is_open path then
    invalid_arg "the path is not closed"
  else
    points_of_path path

let glif_string_of_unicodes ?(indent=0) unicodes=
  let indent_str= String.make indent ' ' in
  unicodes
    |> List.map @@ sprintf "%s<unicode hex=\"%x\" />" indent_str
    |> String.concat "\n"

let glif_string_of_outline_elm ?(indent=0) elm=
  let indent_str= String.make indent ' ' in
  match elm with
  | Component c->
    let attrs=
      c.identifier
        |> Option.map (fun i-> [sprintf "base=\"%s\"" i])
        |> Option.value ~default:[]
      |> List.cons @@ sprintf "yOffset=\"%s\"" (string_of_float c.yOffset)
      |> List.cons @@ sprintf "xOffset=\"%s\"" (string_of_float c.xOffset)
      |> List.cons @@ sprintf "yScale=\"%s\"" (string_of_float c.yScale)
      |> List.cons @@ sprintf "yxScale=\"%s\"" (string_of_float c.yxScale)
      |> List.cons @@ sprintf "xyScale=\"%s\"" (string_of_float c.xyScale)
      |> List.cons @@ sprintf "xScale=\"%s\"" (string_of_float c.xScale)
      |> (fun attrs->
        match c.base with
        | None-> attrs
        | Some base-> attrs |> List.cons @@
          sprintf "base=\"%s\"" base)
      |> String.concat " "
    in
    sprintf "%s<component %s />" indent_str attrs
  | Contour contour->
    let attr= contour.identifier
      |> Option.map (sprintf " identifier=\"%s\" ")
      |> Option.value ~default:""
    in
    let points= contour.points
      |> List.map (fun point-> sprintf "%s"
        (glif_string_of_contour_point ~indent:(indent+2) point))
      |> String.concat "\n"
    in
    sprintf "%s<contour%s>\n%s\n%s</contour>"
      indent_str
      attr
      points
      indent_str

let glif_string_of_t ?(indent=0) glif=
  let step= 2 in
  let indent_0= String.make (indent+step*0) ' '
  and indent_1= String.make (indent+step*1) ' ' in
  let outline=
    let elements= glif.elements
      |> List.map (glif_string_of_outline_elm ~indent:(indent+4))
      |> String.concat "\n"
    in
    sprintf "%s<outline>\n%s\n%s</outline>" indent_1 elements indent_1
  in
  sprintf
"%s<?xml version=\"1.0\" encoding=\"UTF-8\"?>
%s<glyph name=\"%s\" format=\"%d\" formatMinor=\"%d\">
%s<advance width=\"%s\" height=\"%s\" />
%s
%s
%s</glyph>"
    indent_0
    indent_0 glif.name glif.format glif.formatMinor
    indent_1 (string_of_float glif.advance.width) (string_of_float glif.advance.height)
    (glif_string_of_unicodes glif.unicodes)
    outline
    indent_0
