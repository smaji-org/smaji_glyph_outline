(*
 * point.mli
 * -----------
 * Copyright : (c) 2025, smaji.org
 * Copyright : (c) 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)


module PointI : sig
  type cell = int
  type t = { x : cell; y : cell }

  val zero : t
  val to_tuple : t -> cell * cell
  val of_tuple : cell * cell -> t
  val to_string : t -> string
end

module PointF : sig
  type cell = float
  type t = { x : cell; y : cell }

  val zero : t
  val to_pointi : t -> PointI.t
  val of_pointi : PointI.t -> t
  val to_tuple : t -> cell * cell
  val of_tuple : cell * cell -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( *> ) : cell -> t -> t
  val ( /> ) : cell -> t -> t
  val ( <* ) : t -> cell -> t
  val ( >/ ) : t -> cell -> t
  val to_string : t -> string
end

