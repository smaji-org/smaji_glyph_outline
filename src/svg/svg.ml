(*
 * svg.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)

module ViewBox = ViewBox
module Svg_path = Svg_path

type t= {
  viewBox: ViewBox.t;
  paths: Svg_path.t list;
}

let svg_string_of_t ?(indent=0) t=
  let step= 2 in
  let open Printf in
  let viewBox= ViewBox.to_string_svg t.viewBox in
  let paths= t.paths
    |> List.map (fun path->
      sprintf "%s<path d=\"%s\"\n%s/>"
        (String.make (indent+step) ' ')
        (Svg_path.to_string_svg ~indent:(indent+step*2) path)
        (String.make (indent+step) ' ')
      )
    |> String.concat "\n"
  in
  sprintf "%s<svg viewBox=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">\n%s\n%s</svg>"
    (String.make indent ' ')
    viewBox
    paths
    (String.make indent ' ')

let set_viewBox viewBox t=
  { t with viewBox= viewBox }

module Adjust = struct
  let viewBox_reset t=
    let dx= -. t.viewBox.min_x
    and dy= -. t.viewBox.min_y in
    let viewBox= { t.viewBox with min_x= 0.; min_y= 0. } in
    let paths= t.paths |> List.map (Svg_path.Adjust.translate ~dx ~dy) in
    { viewBox; paths }

  let viewBox_fitFrame t= t.paths
    |> Svg_path.get_frame_paths
    |> Option.map @@ fun frame->
      let height= frame.Path.max_y -. frame.min_y
      and width= frame.max_x -. frame.min_x
      and min_x= frame.min_x
      and min_y= frame.min_y in
      ViewBox.{ min_x; min_y; width; height }

  let viewBox_fitFrame_reset t=
    match viewBox_fitFrame t with
    | None-> t
    | Some viewBox->
      { t with viewBox } |> viewBox_reset

  let scale ~x ~y svg=
    let viewBox= svg.viewBox
    and paths= svg.paths in
    let (*viewBox= { viewBox with
      width= viewBox.width *. x;
      height= viewBox.height *. y;
      }
    and*) paths= List.map (Svg_path.Adjust.scale ~x ~y) paths in
    { viewBox; paths }

  let translate ~dx ~dy svg=
    let paths= List.map (Svg_path.Adjust.translate ~dx ~dy) svg.paths in
    { svg with paths }
end

let of_xml_nodes nodes=
  let open Utils in
  let get_paths nodes=
    Ezxmlm.members_with_attr "path" nodes
  in
  let attrs, svg= nodes |> Ezxmlm.member_with_attr "svg" in
  match Ezxmlm.get_attr "viewBox" attrs |> ViewBox.of_string with
  | Some viewBox->
    let paths=
      let container= match xml_member_opt "g" svg with
        | Some g-> g
        | None-> svg
      in
      container
        |> get_paths
        |> List.map (fun (attrs, _)->
          Ezxmlm.get_attr "d" attrs)
        |> List.filter_map Svg_path.of_string
    in
    Some {viewBox; paths}
  | None-> None

let of_string str=
  let _dtd, nodes= Ezxmlm.from_string str in
  of_xml_nodes nodes

let load_file path=
  In_channel.with_open_text path @@ fun chan->
  let _dtd, nodes= Ezxmlm.from_channel chan in
  of_xml_nodes nodes

let load_file_exn path=
  match load_file path with
  | Some svg-> svg
  | None-> failwith "load_file"

