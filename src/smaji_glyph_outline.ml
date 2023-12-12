(*
 * smaji_glyph_outline.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

module Outline= Outline
module Svg= Svg
module Glif= Glif
module Utils= Utils

let glif_of_svg (svg:Svg.t)=
  let name= ""
  and format= 2
  and formatMinor= 0
  and advance=
    let width= svg.viewBox.width
    and height= svg.viewBox.height in
    Glif.{ width; height }
  and unicodes= []
  and elements= svg.paths
    |> List.map @@ List.map (fun sub->
      let identifier= None
      and points= sub
        |> Svg.Path.sub_to_outline
        |> Glif.points_of_outline
      in
      Glif.Contour { identifier; points })
    |> List.concat
  in
  Glif.{
    name;
    format;
    formatMinor;
    advance;
    unicodes;
    elements
  }

let svg_of_glif (glif:Glif.t)=
  let viewBox= Svg.ViewBox.{
    min_x= 0.;
    min_y= 0.;
    width= glif.advance.width;
    height= glif.advance.height;
    }
  in
  let paths= glif.elements
    |> List.filter_map @@ function
      | Glif.Component _component-> None
      (* glif composition is not supported in this low level library,
         please visit project smaji_god for more information. *)
      | Glif.Contour contour-> contour.points
        |> Glif.outline_of_points
        |> Option.map @@ fun outline-> [Svg.Path.sub_of_outline outline]
  in
  Svg.{ viewBox; paths }

