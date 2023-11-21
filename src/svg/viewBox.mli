(*
 * viewBox.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

(** The type of viewBox *)
type t = {
  min_x : float;
  min_y : float;
  width : float;
  height : float;
}

module Parser :
  sig
    val ( let* ) :
      'a Utils.MiniParsec.parser ->
      ('a -> 'b Utils.MiniParsec.parser) ->
      Utils.MiniParsec.state ->
      ('b * Utils.MiniParsec.state, Utils.MiniParsec.error) result
    val string_of_cl : char list -> string
    val space : Utils.MiniParsec.state -> char Utils.MiniParsec.reply
    val spaces : char list Utils.MiniParsec.parser
    val spaces1 :
      Utils.MiniParsec.state ->
      (char list * Utils.MiniParsec.state, Utils.MiniParsec.error) result
    val number_sep :
      Utils.MiniParsec.state ->
      (char list * Utils.MiniParsec.state, Utils.MiniParsec.error) result
    val float1 :
      Utils.MiniParsec.state ->
      (float * Utils.MiniParsec.state, Utils.MiniParsec.error) result
    val float4 :
      Utils.MiniParsec.state ->
      ((float * float * float * float) * Utils.MiniParsec.state,
        Utils.MiniParsec.error)
      result
    val viewBox :
      Utils.MiniParsec.state ->
      (t * Utils.MiniParsec.state, Utils.MiniParsec.error) result
  end

val of_string : string -> t option
(** Parse and return viewBox from string *)

val to_string_hum : t -> string
(** Convert viewBox to human readable string *)

val to_string_svg : t -> string
(** Convert viewBox to svg-formatted string *)

