(*
 * smaji_glyph_outline.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

module Outline = Outline
(** Module used for the basic outline data structure *)

module Svg = Svg
(** Module used for svg supporting *)

module Glif = Glif
(** Module used for glif supporting *)

module Utils = Utils
(** Internal utils *)

val glif_of_svg : Svg.t -> Glif.t
(** Convert from svg to glif *)

val svg_of_glif : Glif.t -> Svg.t
(** Convert from glif to svg *)

