(*
 * path.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

type point = float * float

(** {2 Path command descriptions} *)

type cubic_desc = { ctrl1 : point; ctrl2 : point; end' : point; }
(** The type of description of cubic bézier curve command*)

type s_cubic_desc = { ctrl2 : point; end' : point; }
(** The type of description of subsequent cubic bézier curve command*)

type quadratic_desc = { ctrl : point; end' : point; }
(** The type of description of cubic bézier curve command*)

type s_quadratic_desc = { end' : point; }
(** The type of description of subsequent cubic bézier curve command*)

(** The type of description of elliptical arc curve command*)
type arc_desc = {
  rx : float;       (** x radius of the ellipse *)
  ry : float;       (** y radius of the ellipse *)
  angle : float;    (** rotation in degrees of the ellipse relative to the x-axis *)
  large_arc : bool; (** chose one of the large arc (true) or small arc (false) *)
  sweep : bool;     (** chose one of the clockwise turning arc (true) or counterclockwise turning arc (false) *)
  end' : point;     (** the end point of the arc and also the new current point for the next command *)
}

(** Path commands *)
type command =
  | Cmd_L of point
  | Cmd_l of point
  | Cmd_H of float
  | Cmd_h of float
  | Cmd_V of float
  | Cmd_v of float
  | Cmd_C of cubic_desc
  | Cmd_c of cubic_desc
  | Cmd_S of s_cubic_desc
  | Cmd_s of s_cubic_desc
  | Cmd_Q of quadratic_desc
  | Cmd_q of quadratic_desc
  | Cmd_T of s_quadratic_desc
  | Cmd_t of s_quadratic_desc
  | Cmd_A of arc_desc
  | Cmd_a of arc_desc

type start_point = Absolute of point | Relative of point

(** {2 Point or path command adjustment } *)

val point_add : point -> point -> point

val point_translate : dx:float -> dy:float -> point -> point

val point_scale : x:float -> y:float -> point -> point

val start_point_adjust_point :
  dx:float -> dy:float -> start_point -> start_point

val start_point_adjust_scale :
  x:float -> y:float -> start_point -> start_point

val command_adjust_position : dx:float -> dy:float -> command -> command

val command_adjust_scale : x:float -> y:float -> command -> command

(** {2 Convert the given point or path command to string } *)

(** For each type, there are two versions of converter: with or without [_svg] suffix respectively. The [_svg] version converts the given value to svg-formatted plain text, while the other version converts the given value to human readable verion. *)

val string_of_start_point : start_point -> string

val string_of_start_point_svg : start_point -> string

val string_of_point : point -> string

val string_of_point_svg : string * string -> string

val string_of_cubic_desc : cubic_desc -> string

val string_of_cubic_desc_svg : cubic_desc -> string

val string_of_s_cubic_desc : s_cubic_desc -> string

val string_of_s_cubic_desc_svg : s_cubic_desc -> string

val string_of_quadratic_desc : quadratic_desc -> string

val string_of_quadratic_desc_svg : quadratic_desc -> string

val string_of_arc_desc : arc_desc -> string

val string_of_arc_desc_svg : arc_desc -> string

val string_of_command : command -> string

val string_of_command_svg : command -> string

(** {2 Sub segment and path descriptions} *)

type sub = { start : start_point; segments : command list; }

type t = sub list

(** {2 Frame and frame arithemetic } *)

type frame = { px : float; nx : float; py : float; ny : float; }

val frame_merge : frame -> frame -> frame

val string_of_frame : frame -> string

val frame_update : point -> frame -> frame

val get_frame_sub : ?previous:point -> sub -> frame * point

val get_frame : t -> frame option

val get_frame_paths : t list -> frame option

(** {2 Adjust sub segment or path } *)
module Adjust :
  sig
    val translate_sub : dx:float -> dy:float -> sub -> sub
    val scale_sub : x:float -> y:float -> sub -> sub
    val translate : dx:float -> dy:float -> t -> t
    val scale : x:float -> y:float -> t -> t
  end

(** {2 Convert sub segment or path to string } *)

val sub_to_string_hum : sub -> string

val to_string_hum : t -> string

val sub_to_string_svg :
  ?previous:point -> ?indent:int -> sub -> string

val to_string_svg : ?indent:int -> t -> string

(** {2 Parse and return path commands } *)

module Parser :
sig
  open Utils
  type command =
      Cmd_M of point list
    | Cmd_m of point list
    | Cmd_L of point list
    | Cmd_l of point list
    | Cmd_H of float list
    | Cmd_h of float list
    | Cmd_V of float list
    | Cmd_v of float list
    | Cmd_C of cubic_desc list
    | Cmd_c of cubic_desc list
    | Cmd_S of s_cubic_desc list
    | Cmd_s of s_cubic_desc list
    | Cmd_Q of quadratic_desc list
    | Cmd_q of quadratic_desc list
    | Cmd_T of point list
    | Cmd_t of point list
    | Cmd_A of arc_desc list
    | Cmd_a of arc_desc list
    | Cmd_Z
    | Cmd_z
  val string_of_cl : char list -> string
  val ( let* ) :
    'a MiniParsec.parser ->
    ('a -> 'b MiniParsec.parser) ->
    MiniParsec.state -> ('b * MiniParsec.state, MiniParsec.error) result
  val space : MiniParsec.state -> char MiniParsec.reply
  val spaces : char list MiniParsec.parser
  val number_sep :
    MiniParsec.state ->
    (char list * MiniParsec.state, MiniParsec.error) result
  val float1 :
    MiniParsec.state -> (float * MiniParsec.state, MiniParsec.error) result
  val float2 :
    MiniParsec.state ->
    ((float * float) * MiniParsec.state, MiniParsec.error) result
  val float4 :
    MiniParsec.state ->
    ((float * float * float * float) * MiniParsec.state, MiniParsec.error)
    result
  val float6 :
    MiniParsec.state ->
    ((float * float * float * float * float * float) * MiniParsec.state,
     MiniParsec.error)
    result
  val point :
    MiniParsec.state ->
    ((float * float) * MiniParsec.state, MiniParsec.error) result
  val tag_M :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_m :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_L :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_l :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_H :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_h :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_V :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_v :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_C :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_c :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_S :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_s :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_Q :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_q :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_T :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_t :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_A :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_a :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_Z :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val tag_z :
    MiniParsec.state ->
    (char * MiniParsec.state, MiniParsec.pos * string) result
  val arc_desc :
    MiniParsec.state ->
    (arc_desc * MiniParsec.state, MiniParsec.error) result
  val cmd_M :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_m :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_L :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_l :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_H :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_h :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_V :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_v :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_C :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_c :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_S :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_s :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_Q :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_q :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_T :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_t :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_A :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_a :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_Z :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val cmd_z :
    MiniParsec.state -> (command * MiniParsec.state, MiniParsec.error) result
  val path :
    MiniParsec.state ->
    (command list * MiniParsec.state, MiniParsec.error) result
end

val of_string : string -> t option
(** Parse the d attribute and return the path *)

val sub_of_outline : Outline.path -> sub
(** Convert [Outline.path] to [sub] command *)

val sub_to_outline : ?prev:point -> sub -> Outline.path
(** Convert [sub] command to [Outline.path] *)

