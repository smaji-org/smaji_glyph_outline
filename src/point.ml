(*
 * point.ml
 * -----------
 * Copyright : (c) 2025, smaji.org
 * Copyright : (c) 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)


module PointI = struct
  type cell = int
  type t= {
    x: cell;
    y: cell;
  }

  let abs t= {
    x= abs(t.x);
    y= abs(t.y);
  }

  let zero= {
    x= 0;
    y= 0;
  }

  let ( *> ) f p= { x= f * p.x; y= f * p.y }
  let ( /> ) f p= { x= f / p.x; y= f / p.y }
  let ( <* ) = Fun.flip ( *> )
  let ( >/ ) = Fun.flip ( /> )

  let ( + ) p1 p2= { x= p1.x+p2.x; y= p1.y+p2.y }
  let ( - ) p1 p2= { x= p1.x-p2.x; y= p1.y-p2.y }
  let ( * ) p1 p2= { x= p1.x*p2.x; y= p1.y*p2.y }
  let ( / ) p1 p2= { x= p1.x/p2.x; y= p1.y/p2.y }

  let to_tuple p= (p.x,p.y)
  let of_tuple (x,y)= {x;y}
  let to_string p= Printf.sprintf "(%d,%d)" p.x p.y

  let distance (p1:t) (p2:t)=
    let difference= (p2 - p1) in
    let x= difference.x
    and y= difference.y in
    let open Stdlib in
    x * x + y * y |> Float.of_int |> Float.sqrt

  let perimeter points=
    let rec perimeter fst acc prev points=
      match points with
      | []->
        let acc= acc +. distance prev fst in
        acc
      | point::tl->
        let acc= acc +. distance point prev in
        perimeter fst acc point tl
    in
    match points with
    | [fst;snd]-> distance fst snd
    | fst::snd::thd::tl-> perimeter fst 0. fst (snd::thd::tl)
    | _-> 0.

end

module PointF = struct
  type cell = float
  type t= {
    x: cell;
    y: cell;
  }

  let abs t= {
    x= abs_float(t.x);
    y= abs_float(t.y);
  }

  let zero= {
    x= 0.;
    y= 0.;
  }

  let to_pointi p=
    let x= int_of_float p.x
    and y= int_of_float p.y in
    PointI.{ x; y }

  let of_pointi (p:PointI.t)=
    let x= float_of_int p.x
    and y= float_of_int p.y in
    { x; y }

  let to_tuple p=
    (p.x, p.y)
  let of_tuple (x,y)= {x;y}

  let ( *> ) f p= { x= f *. p.x; y= f *. p.y }
  let ( /> ) f p= { x= f /. p.x; y= f /. p.y }
  let ( <* ) = Fun.flip ( *> )
  let ( >/ ) = Fun.flip ( /> )

  let ( + ) p1 p2= { x= p1.x+.p2.x; y= p1.y+.p2.y }
  let ( - ) p1 p2= { x= p1.x-.p2.x; y= p1.y-.p2.y }
  let ( * ) p1 p2= { x= p1.x*.p2.x; y= p1.y*.p2.y }
  let ( / ) p1 p2= { x= p1.x/.p2.x; y= p1.y/.p2.y }

  let to_string p= Printf.sprintf "(%s,%s)"
    (Utils.string_of_float p.x)
    (Utils.string_of_float p.y)

  let distance (p1:t) (p2:t)=
    let difference= (p2 - p1) in
    let x= difference.x
    and y= difference.y in
    x *. x +. y *. y |> Float.sqrt

  let perimeter points=
    let rec perimeter fst acc prev points=
      match points with
      | []->
        let acc= acc +. distance prev fst in
        acc
      | point::tl->
        let acc= acc +. distance point prev in
        perimeter fst acc point tl
    in
    match points with
    | [fst;snd]-> distance fst snd
    | fst::snd::thd::tl-> perimeter fst 0. fst (snd::thd::tl)
    | _-> 0.

end

