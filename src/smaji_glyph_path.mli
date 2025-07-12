(*
 * smaji_glyph_path.mli
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)

(** A closed path is an outline. In a glif file, all its contours, by definition, are outlines. *)

module Path = Path
(** Module used for basic path data structure *)

module Svg = Svg
(** Module used for svg path *)

module Glif = Glif
(** Module used for glif outline *)

module Utils = Utils
(** Internal utils *)

val glif_of_svg : Svg.t -> Glif.t option
(** Convert from svg to glif *)

val glif_of_svg_exn : Svg.t -> Glif.t
(** Convert from svg to glif, Invalid_argument is raised for not closed svg path *)

val svg_of_glif : Glif.t -> Svg.t
(** Convert from glif to svg *)

