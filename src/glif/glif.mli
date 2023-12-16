(*
 * glif.mli
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

type point = Outline.point

(** The type of description of cubic bézier curve command*)
type cubic_desc = { ctrl1 : point; ctrl2 : point; end' : point; }

(** The type of description of quadratic bézier curve command*)
type quadratic_desc = { ctrl : point; end' : point; }

(** The type of compoment, which is one of two types of the child element of the outline element. *)
type component = {
  base : string option; (** name of the base glyph *)
  xScale : float;
  xyScale : float;
  yxScale : float;
  yScale : float;
  xOffset : float;
  yOffset : float;
  identifier : string option; (** unique identifier for the component. This attribute is not required and should only be added to components as needed. However, once an identifier has been assigned to a component it must not be unnecessarily removed or changed. *)
}
(** xScale, xyScale, yxScale, yScale, xOffset, yOffset taken together in that order form an Affine transformation matrix, to be used to transform the base glyph. The default matrix is [1 0 0 1 0 0], the identity transformation. *)

type contour_point_type =
  | Line (** Draw a straight line from the previous point to this point. The previous point must be a move, a line, a curve or a qcurve. It must not be an offcurve. *)
  | Offcurve (** This point is part of a curve segment that goes up to the next point that is either a curve or a qcurve. *)
  | Curve (** Draw a cubic bezier curve from the last non-offcurve point to this point. The number of offcurve points can be zero, one or two. If the number of offcurve points is zero, a straight line is drawn. If it is one, a quadratic curve is drawn. If it is two, a regular cubic bezier is drawn. *)
  | Qcurve (** Draw a cubic bezier curve from the last non-offcurve point to this point. The number of offcurve points preceding this point can be zero, in which case a straight line is drawn from the last non-offcurve point to this point. *)

(** Parses the string and returns the corresponding contour_point_type. *)
val contour_point_type_of_string : string -> contour_point_type

(** Returns the string representation of the contour_point_type. *)
val string_of_contour_point_type : contour_point_type -> string

(** The type of contour_point *)
type contour_point = {
  x : float; (** coordinate x *)
  y : float; (** coordinate y *)
  point_type : contour_point_type;
}

(** The type of contour element *)
type contour = {
  identifier : string option; (** Unique identifier for the contour. This attribute is not required and should only be added to contours as needed. However, once an identifier has been assigned to a contour it must not be unnecessarily removed or changed. *)
  points : contour_point list; (** Points to form a contour. *)
}

(** Returns the human-readable format string of the contour_point. *)
val string_of_contour_point : contour_point -> string

(** Returns the glif xml format string of the contour_point. *)
val glif_string_of_contour_point : ?indent:int -> contour_point -> string

(** Horizontal and vertical metrics *)
type advance = { width : float; height : float; }

(** Unicode code point *)
type unicodes = int list

(** The type of the sub elements of the outline element. *)
type outline_elm =
  | Component of component (** Insert another glyph as part of the outline *)
  | Contour of contour (** Contour description *)

(** The type of glif *)
type t = {
  name : string; (** The name of the glyph. It must be at least one character long. There is no maximum name length. Names must not contain control characters. *)
  format : int; (** The major format version. *)
  formatMinor : int; (** The minor format version. Optional if the minor version is 0. *)
  advance : advance; (** Horizontal and vertical metrics. *)
  unicodes : unicodes; (** Unicode code points. *)
  elements : outline_elm list; (** sub elements included in the outline element. *)
}

(** [load_file path] returns [t] if the file specified by path is read and parsed successfully, otherwise exn Failure is emitted. *)
val load_file_exn : string -> t

(** [load_file path] returns [t] if the file specified by path is read and parsed successfully, otherwise [None] is returned. *)
val load_file : string -> t option

(** [of_string string] reads and parses the string and return the represented glif. *)
val of_string : string -> t

(** Convert contour_point list to the general outline path. *)
val outline_of_points : contour_point list -> Outline.path option

(** Convert [Outline.path] to a list of contour_point. *)
val points_of_outline : Outline.path -> contour_point list

(** Returns the glif xml formatted string of the given unicode list. *)
val glif_string_of_unicodes : ?indent:int -> int list -> string

(** Returns the glif xml formatted string of the given outline_elm. *)
val glif_string_of_outline_elm : ?indent:int -> outline_elm -> string

(** Returns the glif xml formatted string of the given glif. *)
val glif_string_of_t : ?indent:int -> t -> string

