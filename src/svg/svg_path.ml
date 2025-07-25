(*
 * path.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)


open Utils

module Point= Point.PointF
type point= Point.t

type cubic_desc= {
  ctrl1: point;
  ctrl2: point;
  end': point
}

type s_cubic_desc= {
  ctrl2: point;
  end': point
}

type quadratic_desc= {
  ctrl: point;
  end': point
}

type s_quadratic_desc = { end' : point; }

type arc_desc= {
  rx: float;
  ry: float;
  angle: float;
  large_arc: bool;
  sweep: bool;
  end': point;
}

type command=
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

type start_point=
  | Absolute of point
  | Relative of point

let point_translate ?(dx=0.) ?(dy=0.) p=
  Point.({x=dx;y=dy} + p)
let point_scale ?(x=1.) ?(y=1.) p= Point.({x;y} * p)

let start_point_adjust_point ~dx ~dy= function
  | Absolute point-> Absolute (point_translate ~dx ~dy point)
  | Relative _ as r-> r

let start_point_adjust_scale ~x ~y= function
  | Absolute point-> Absolute (point_scale ~x ~y point)
  | Relative point-> Relative (point_scale ~x ~y point)

let command_adjust_position ~dx ~dy cmd=
  let adj_p= point_translate ~dx ~dy in
  match cmd with
  | Cmd_L point-> Cmd_L (adj_p point)
  | Cmd_l _ as l-> l
  | Cmd_H h-> Cmd_H (h +. dx)
  | Cmd_h _ as h-> h
  | Cmd_V v-> Cmd_V (v +. dy)
  | Cmd_v _ as v-> v

  | Cmd_C desc-> Cmd_C {
      ctrl1= adj_p desc.ctrl1;
      ctrl2= adj_p desc.ctrl2;
      end'= adj_p desc.end';
    }
  | Cmd_c _ as c-> c
  | Cmd_S desc-> Cmd_S {
      ctrl2= adj_p desc.ctrl2;
      end'= adj_p desc.end';
    }
  | Cmd_s _ as s-> s

  | Cmd_Q desc-> Cmd_Q {
      ctrl= adj_p desc.ctrl;
      end'= adj_p desc.end';
    }
  | Cmd_q _ as q-> q
  | Cmd_T desc-> Cmd_T { end'= adj_p desc.end' }
  | Cmd_t _ as t-> t

  | Cmd_A desc-> Cmd_A { desc with
      rx= desc.rx +. dx;
      ry= desc.ry +. dy;
      end'= adj_p desc.end';
    }
  | Cmd_a _ as a-> a

let command_adjust_scale ~x ~y cmd=
  let scale_p= point_scale ~x ~y in
  match cmd with
  | Cmd_L point-> Cmd_L (scale_p point)
  | Cmd_l point-> Cmd_l (scale_p point)
  | Cmd_H h-> Cmd_H (h *. x)
  | Cmd_h h-> Cmd_h (h *. x)
  | Cmd_V v-> Cmd_V (v *. y)
  | Cmd_v v-> Cmd_v (v *. y)

  | Cmd_C desc-> Cmd_C {
      ctrl1= scale_p desc.ctrl1;
      ctrl2= scale_p desc.ctrl2;
      end'= scale_p desc.end';
    }
  | Cmd_c desc-> Cmd_c {
      ctrl1= scale_p desc.ctrl1;
      ctrl2= scale_p desc.ctrl2;
      end'= scale_p desc.end';
    }
  | Cmd_S desc-> Cmd_S {
      ctrl2= scale_p desc.ctrl2;
      end'= scale_p desc.end';
    }
  | Cmd_s desc-> Cmd_s {
      ctrl2= scale_p desc.ctrl2;
      end'= scale_p desc.end';
    }

  | Cmd_Q desc-> Cmd_Q {
      ctrl= scale_p desc.ctrl;
      end'= scale_p desc.end';
    }
  | Cmd_q desc-> Cmd_q {
      ctrl= scale_p desc.ctrl;
      end'= scale_p desc.end';
    }
  | Cmd_T desc-> Cmd_T { end'= scale_p desc.end' }
  | Cmd_t desc-> Cmd_t { end'= scale_p desc.end' }

  | Cmd_A desc-> Cmd_A { desc with
      rx= desc.rx *. x;
      ry= desc.ry *. y;
      end'= scale_p desc.end';
    }
  | Cmd_a desc-> Cmd_a { desc with
      rx= desc.rx *. x;
      ry= desc.ry *. y;
      end'= scale_p desc.end';
    }

open Printf

let string_of_start_point= function
  | Absolute p-> sprintf "Absolute %s,%s" (string_of_float p.x) (string_of_float p.y)
  | Relative p-> sprintf "Relative %s,%s" (string_of_float p.x) (string_of_float p.y)

let string_of_start_point_svg= function
  | Absolute p-> sprintf "M %s,%s" (string_of_float p.x) (string_of_float p.y)
  | Relative p-> sprintf "m %s,%s" (string_of_float p.x) (string_of_float p.y)

let string_of_point (p:point)= sprintf "%s,%s"
  (string_of_float p.x)
  (string_of_float p.y)

let string_of_point_svg = function (x, y)-> sprintf "%s,%s" x y

let string_of_cubic_desc desc=
  sprintf "{ctrl1: %s; ctrl2: %s; end: %s}"
    (string_of_point desc.ctrl1)
    (string_of_point desc.ctrl2)
    (string_of_point desc.end')

let string_of_cubic_desc_svg desc=
  sprintf "%s,%s,%s,%s,%s,%s"
    (string_of_float desc.ctrl1.x)
    (string_of_float desc.ctrl1.y)
    (string_of_float desc.ctrl2.x)
    (string_of_float desc.ctrl2.y)
    (string_of_float desc.end'.x)
    (string_of_float desc.end'.y)

let string_of_s_cubic_desc desc=
  sprintf "{ctrl2: %s; end: %s}"
    (string_of_point desc.ctrl2)
    (string_of_point desc.end')

let string_of_s_cubic_desc_svg desc=
  sprintf "%s,%s,%s,%s"
    (string_of_float desc.ctrl2.x)
    (string_of_float desc.ctrl2.y)
    (string_of_float desc.end'.x)
    (string_of_float desc.end'.y)

let string_of_quadratic_desc desc=
  sprintf "{ctrl: %s; end: %s}"
    (string_of_point desc.ctrl)
    (string_of_point desc.end')

let string_of_quadratic_desc_svg desc=
  sprintf "%s,%s,%s,%s"
    (string_of_float desc.ctrl.x)
    (string_of_float desc.ctrl.y)
    (string_of_float desc.end'.x)
    (string_of_float desc.end'.y)

let string_of_arc_desc desc=
  sprintf "{rx: %s; ry: %s; angle: %s; large_arc: %b; sweep: %b; end: %s}"
    (string_of_float desc.rx)
    (string_of_float desc.ry)
    (string_of_float desc.angle)
    desc.large_arc
    desc.sweep
    (string_of_point desc.end')

let string_of_arc_desc_svg desc=
  let boot_to_int= function true-> 1 | false-> 0 in
  sprintf "%s,%s,%s,%d,%d,%s,%s"
    (string_of_float desc.rx)
    (string_of_float desc.ry)
    (string_of_float desc.angle)
    (boot_to_int desc.large_arc)
    (boot_to_int desc.sweep)
    (string_of_float desc.end'.x)
    (string_of_float desc.end'.y)

let string_of_command= function
  | Cmd_L point-> point |> string_of_point |> sprintf "L %s"
  | Cmd_l point-> point |> string_of_point |> sprintf "l %s"
  | Cmd_H float-> float |> string_of_float |> sprintf "H %s"
  | Cmd_h float-> float |> string_of_float |> sprintf "h %s"
  | Cmd_V float-> float |> string_of_float |> sprintf "V %s"
  | Cmd_v float-> float |> string_of_float |> sprintf "v %s"

  | Cmd_C cubic_desc-> cubic_desc |> string_of_cubic_desc |> sprintf "C %s"
  | Cmd_c cubic_desc-> cubic_desc |> string_of_cubic_desc |> sprintf "c %s"
  | Cmd_S s_cubic_desc-> s_cubic_desc |> string_of_s_cubic_desc |> sprintf "S %s"
  | Cmd_s s_cubic_desc-> s_cubic_desc |> string_of_s_cubic_desc |> sprintf "s %s"

  | Cmd_Q quadratic_desc-> quadratic_desc |> string_of_quadratic_desc |> sprintf "Q %s"
  | Cmd_q quadratic_desc-> quadratic_desc |> string_of_quadratic_desc |> sprintf "q %s"
  | Cmd_T desc-> desc.end' |> string_of_point |> sprintf "T %s"
  | Cmd_t desc-> desc.end' |> string_of_point |> sprintf "t %s"

  | Cmd_A arc_desc-> arc_desc |> string_of_arc_desc |> sprintf "A %s"
  | Cmd_a arc_desc-> arc_desc |> string_of_arc_desc |> sprintf "a %s"


let string_of_command_svg= function
  | Cmd_L point-> point |> string_of_point |> sprintf "L %s"
  | Cmd_l point-> point |> string_of_point |> sprintf "l %s"
  | Cmd_H float-> float |> string_of_float |> sprintf "H %s"
  | Cmd_h float-> float |> string_of_float |> sprintf "h %s"
  | Cmd_V float-> float |> string_of_float |> sprintf "V %s"
  | Cmd_v float-> float |> string_of_float |> sprintf "v %s"

  | Cmd_C cubic_desc-> cubic_desc |> string_of_cubic_desc_svg |> sprintf "C %s"
  | Cmd_c cubic_desc-> cubic_desc |> string_of_cubic_desc_svg |> sprintf "c %s"
  | Cmd_S s_cubic_desc-> s_cubic_desc |> string_of_s_cubic_desc_svg |> sprintf "S %s"
  | Cmd_s s_cubic_desc-> s_cubic_desc |> string_of_s_cubic_desc_svg |> sprintf "s %s"

  | Cmd_Q quadratic_desc-> quadratic_desc |> string_of_quadratic_desc_svg |> sprintf "Q %s"
  | Cmd_q quadratic_desc-> quadratic_desc |> string_of_quadratic_desc_svg |> sprintf "q %s"
  | Cmd_T desc-> desc.end' |> string_of_point |> sprintf "T %s"
  | Cmd_t desc-> desc.end' |> string_of_point |> sprintf "t %s"

  | Cmd_A arc_desc-> arc_desc |> string_of_arc_desc_svg |> sprintf "A %s"
  | Cmd_a arc_desc-> arc_desc |> string_of_arc_desc_svg |> sprintf "a %s"


type sub= {
  start: start_point;
  segments: command list;
}

type t= sub list

(*
let get_frame_sub ?(prev=Point.{x=0.;y=0.}) sub=
  let frame=
    match sub.start with
    | Absolute p->
      let px= p.x
      and nx= p.x
      and py= p.y
      and ny= p.y in
      { px; nx; py; ny }
    | Relative p->
      let x= prev.x
      and y= prev.y in
      let px= x +. p.x
      and nx= x +. p.x
      and py= y +. p.y
      and ny= y +. p.y in
      { px; nx; py; ny }
  in
  let frame, _prev, prev_end=ListLabels.fold_left
    sub.segments
    ~init:(
      let spaceholder= Cmd_l {x=0.;y=0.} in
      frame, spaceholder, {x= frame.nx; y= frame.ny})
    ~f:(fun acc command->
      let (frame, _prev, prev_end)= acc in
      match command with
      | Cmd_L point->
        let frame= frame_update point frame in
        (frame, command, point)
      | Cmd_l point->
        let point= Point.(prev_end + point) in
        let frame= frame_update point frame in
        (frame, command, point)
      | Cmd_H x->
        let point= {prev_end with x} in
        let frame= frame_update point frame in
        (frame, command, point)
      | Cmd_h dx->
        let point= point_translate ~dx prev_end in
        let frame= frame_update point frame in
        (frame, command, point)
      | Cmd_V y->
        let point= {prev_end with y} in
        let frame= frame_update point frame in
        (frame, command, point)
      | Cmd_v dy->
        let point= point_translate ~dy prev_end in
        let frame= frame_update point frame in
        (frame, command, point)

      | Cmd_C desc->
        let frame= frame
          |> frame_update desc.ctrl1
          |> frame_update desc.ctrl2
          |> frame_update desc.end' in
        (frame, command, desc.end')
      | Cmd_c desc->
        let open Point in
        let ctrl1= desc.ctrl1 + prev_end
        and ctrl2= desc.ctrl2 + prev_end
        and end'= desc.end' + prev_end in
        let frame= frame
          |> frame_update ctrl1
          |> frame_update ctrl2
          |> frame_update end' in
        (frame, command, end')
      | Cmd_S desc->
        let frame= frame
          |> frame_update desc.ctrl2
          |> frame_update desc.end' in
        (frame, command, desc.end')
      | Cmd_s desc->
        let open Point in
        let ctrl2= desc.ctrl2 + prev_end
        and end'= desc.end' + prev_end in
        let frame= frame
          |> frame_update ctrl2
          |> frame_update end' in
        (frame, command, end')

      | Cmd_Q desc->
        let frame= frame |> frame_update desc.ctrl |> frame_update desc.end' in
        (frame, command, desc.end')
      | Cmd_q desc->
        let open Point in
        let ctrl= desc.ctrl + prev_end
        and end'= desc.end' + prev_end in
        let frame= frame |> frame_update ctrl |> frame_update end' in
        (frame, command, end')
      | Cmd_T desc->
        let frame= frame_update desc.end' frame in
        (frame, command, desc.end')
      | Cmd_t desc->
        let open Point in
        let point= prev_end + desc.end' in
        let frame= frame_update point frame in
        (frame, command, point)

      | Cmd_A arc_desc-> (frame, command, arc_desc.end')
      | Cmd_a arc_desc->
        let point= Point.(prev_end + arc_desc.end') in
        (frame, command, point)
      )
  in
  (frame, prev_end)
*)

let sub_of_path (path:Path.t)=
  let start= Absolute path.start
  and segments= path.segments |> List.map @@ function
    | Path.Line point-> Cmd_L point
    | Qcurve { ctrl; end'; }-> Cmd_Q { ctrl; end' }
    | Ccurve { ctrl1; ctrl2; end'; }-> Cmd_C { ctrl1; ctrl2; end' }
    | SQcurve end'-> Cmd_T { end' }
    | SCcurve { ctrl; end'; }-> Cmd_S { ctrl2= ctrl; end' }
  in
  { start; segments }

let sub_to_path ?(straighten=false) ?(prev=Point.zero) sub=
  let[@tail_mod_cons] rec to_path prev segments=
    let open Point in
    match segments with
    | []-> []
    | Cmd_L point :: tl-> Path.Line point :: to_path point tl
    | Cmd_l point :: tl->
      let next= prev + point in
      Line next :: to_path next tl
    | Cmd_H x :: tl->
      let next= {x; y= prev.y} in
      Line next :: to_path next tl
    | Cmd_h dx :: tl->
      let next= {prev with x= prev.x+.dx} in
      Line next :: to_path next tl
    | Cmd_V y :: tl->
      let next= {x= prev.x; y} in
      Line next :: to_path next tl
    | Cmd_v dy :: tl->
      let next= {prev with y= prev.y+.dy} in
      Line next :: to_path next tl
    | Cmd_C {ctrl1;ctrl2;end'} :: tl->
      Ccurve {ctrl1;ctrl2;end'} :: to_path end' tl
    | Cmd_c {ctrl1;ctrl2;end'} :: tl->
      let ctrl1= prev + ctrl1
      and ctrl2= prev + ctrl2
      and end'= prev + end' in
      Ccurve {ctrl1; ctrl2; end'} :: to_path end' tl
    | Cmd_S {ctrl2; end'} :: tl->
      SCcurve {ctrl= ctrl2; end'} :: to_path end' tl
    | Cmd_s {ctrl2; end'} :: tl->
      let ctrl= prev + ctrl2
      and end'= prev + end' in
      SCcurve {ctrl; end'} :: to_path end' tl

    | Cmd_Q {ctrl; end'} :: tl->
      Qcurve {ctrl; end'} :: to_path end' tl
    | Cmd_q {ctrl; end'} :: tl->
      let ctrl= prev + ctrl
      and end'= prev + end' in
      Qcurve {ctrl; end'} :: to_path end' tl
    | Cmd_T {end'} :: tl-> SQcurve end' :: to_path end' tl
    | Cmd_t {end'} :: tl->
      let end'= prev + end' in
      SQcurve end' :: to_path end' tl

    | Cmd_A arc_desc :: tl->
      if straighten then
        let end'= arc_desc.end' in
        Path.Line end':: to_path end' tl
      else
        invalid_arg "elliptical arc unsupported"
    | Cmd_a arc_desc :: tl->
      if straighten then
        let end'= arc_desc.end' + prev in
        Path.Line end':: to_path end' tl
      else
        invalid_arg "elliptical arc unsupported"
  in
  let start=
    match sub.start with
    | Absolute point-> point
    | Relative point-> Point.(prev + point)
  in
  let segments= sub.segments |> to_path start in
  Path.{ start; segments }

let get_frame_sub ?(straighten=false) ?(prev=Point.zero) sub=
  sub |> sub_to_path ~straighten ~prev |> Path.frame_algo_svg


let get_frame t=
  match t with
  | []-> None
  | hd::tl->
    let frame, prev= get_frame_sub hd in
    let frame, _=
      ListLabels.fold_left tl
        ~init:(frame, prev)
        ~f:(fun (frame, prev) path->
          let (frame_new, prev)= get_frame_sub ~prev path in
          (Path.frame_merge frame frame_new, prev)
          )
    in Some frame

let get_frame_paths paths=
  ListLabels.fold_left paths
    ~init:None
    ~f:(fun acc path->
      match get_frame path with
      | Some frame->
        (match acc with
        | Some acc-> Some (Path.frame_merge acc frame)
        | None-> Some frame)
      | None-> acc)

(** {2 Module Adjust } *)
module Adjust = struct
  let translate_sub ~dx ~dy sub= {
    start= start_point_adjust_point ~dx ~dy sub.start;
    segments= List.map (command_adjust_position ~dx ~dy) sub.segments;
  }

  let scale_sub ~x ~y sub= {
    start= start_point_adjust_scale ~x ~y sub.start;
    segments= List.map (command_adjust_scale ~x ~y) sub.segments;
  }

  let translate ~dx ~dy t= List.map (translate_sub ~dx ~dy) t

  let scale ~x ~y t= List.map (scale_sub ~x ~y) t
end

let sub_to_string_hum sub=
  let start= string_of_start_point sub.start
  and segments=
    sub.segments |> List.map string_of_command
  in
  String.concat "; " (start::segments)

let to_string_hum t=
  t |> List.map sub_to_string_hum |> String.concat ";; "

let sub_to_string_svg ?prev ?(indent=0) sub=
  let indent= String.make indent ' ' in
  let start= match sub.start with
    | Absolute p-> sprintf "\n%sM %s,%s"
      indent
      (string_of_float p.x)
      (string_of_float p.y)
    | Relative p->
      match prev with
      | Some Point.{x;y}->
        sprintf "\n%sM %s,%s" indent
          (string_of_float (x+.p.x))
          (string_of_float (y+.p.y))
      | None->
        sprintf "\n%sm %s,%s"
          indent
          (string_of_float p.x)
          (string_of_float p.y)
  and segments=
    sub.segments |> List.map string_of_command_svg
  in
  String.concat (sprintf "\n%s" indent) (start::segments @ ["Z"])


let to_string_svg ?(indent=0) t= t
  |> List.map (sub_to_string_svg ~indent)
  |> String.concat "\n"

module Parser = struct
  type command=
    | Cmd_M of point list
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

    | Cmd_Z | Cmd_z


  open Utils.MiniParsec

  let string_of_cl cl= String.concat "" (List.map (String.make 1) cl)

  let ( let* )= bind

  let space= char ' ' <|> char '\t' <|> (newline |>> (fun _-> '\n'))

  let spaces= many space

  let number_sep= spaces >> option (char ',') >> spaces

  let float1=
    let* neg= option (char '-') in
    let* integer= many1 num_dec << option (char '.') in
    let* fractional= option (many1 num_dec) in
    let neg= Option.is_some neg in
    let integer= integer |> string_of_cl |> float_of_string in
    let fractional= match fractional with
      | None-> 0.
      | Some cl-> "." ^ (string_of_cl cl) |> float_of_string
    in
    let num= integer +. fractional in
    let result= if neg then -. num else num in
    return result

  let float2=
    let* point1= float1 in
    let* _= number_sep in
    let* point2= float1 in
    return (point1, point2)

  let float4=
    let* point1= float1 in
    let* _= number_sep in
    let* point2= float1 in
    let* _= number_sep in
    let* point3= float1 in
    let* _= number_sep in
    let* point4= float1 in
    return (point1, point2, point3, point4)

  let float6=
    let* point1= float1 in
    let* _= number_sep in
    let* point2= float1 in
    let* _= number_sep in
    let* point3= float1 in
    let* _= number_sep in
    let* point4= float1 in
    let* _= number_sep in
    let* point5= float1 in
    let* _= number_sep in
    let* point6= float1 in
    return (point1, point2, point3, point4, point5, point6)

  let point = let* (x,y)= float2 in return Point.{x;y}

  let tag_M= char 'M'
  let tag_m= char 'm'

  let tag_L= char 'L'
  let tag_l= char 'l'
  let tag_H= char 'H'
  let tag_h= char 'h'
  let tag_V= char 'V'
  let tag_v= char 'v'

  let tag_C= char 'C'
  let tag_c= char 'c'
  let tag_S= char 'S'
  let tag_s= char 's'

  let tag_Q= char 'Q'
  let tag_q= char 'q'
  let tag_T= char 'T'
  let tag_t= char 't'

  let tag_A= char 'A'
  let tag_a= char 'a'

  let tag_Z= char 'Z'
  let tag_z= char 'z'

  let arc_desc=
    let* rx= float1 in
    let* _= number_sep in
    let* ry= float1 in
    let* _= number_sep in
    let* angle= float1 in
    let* _= number_sep in
    let* large_arc= num_dec |>> (<>) '0' in
    let* _= number_sep in
    let* sweep= num_dec |>> (<>) '0' in
    let* _= number_sep in
    let* end'= point in
    return {
      rx;
      ry;
      angle;
      large_arc;
      sweep;
      end'
    }

  let cmd_M=
    let* _= tag_M in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_M points
  let cmd_m=
    let* _= tag_m in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_m points

  let cmd_L=
    let* _= tag_L in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_L points
  let cmd_l=
    let* _= tag_l in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_l points
  let cmd_H=
    let* _= tag_H in
    let* numbers= sepStartBy1 (option spaces) float1 in
    return @@ Cmd_H numbers
  let cmd_h=
    let* _= tag_h in
    let* numbers= sepStartBy1 (option spaces) float1 in
    return @@ Cmd_h numbers
  let cmd_V=
    let* _= tag_V in
    let* numbers= sepStartBy1 (option spaces) float1 in
    return @@ Cmd_V numbers
  let cmd_v=
    let* _= tag_v in
    let* numbers= sepStartBy1 (option spaces) float1 in
    return @@ Cmd_v numbers

  let cmd_C=
    let* _= tag_C in
    let* points= sepStartBy1 (option spaces) float6 in
    let descs= points |> List.map (fun (p1, p2, p3, p4, p5, p6)->
      { ctrl1={x=p1; y=p2};
        ctrl2={x=p3; y=p4};
        end'={x=p5; y=p6};
      })
    in
    return @@ Cmd_C descs
  let cmd_c=
    let* _= tag_c in
    let* points= sepStartBy1 (option spaces) float6 in
    let descs= points |> List.map (fun (p1, p2, p3, p4, p5, p6)->
      { ctrl1={x=p1; y=p2};
        ctrl2={x=p3; y=p4};
        end'={x=p5; y=p6};
      })
    in
    return @@ Cmd_c descs
  let cmd_S=
    let* _= tag_S in
    let* points= sepStartBy1 (option spaces) float4 in
    let descs= points |> List.map (fun (p1, p2, p3, p4)->
      { ctrl2={x=p1; y=p2};
        end'={x=p3; y=p4};
      })
    in
    return @@ Cmd_S descs
  let cmd_s=
    let* _= tag_s in
    let* points= sepStartBy1 (option spaces) float4 in
    let descs= points |> List.map (fun (p1, p2, p3, p4)->
      { ctrl2={x=p1; y=p2};
        end'={x=p3; y=p4};
      })
    in
    return @@ Cmd_s descs


  let cmd_Q=
    let* _= tag_Q in
    let* points= sepStartBy1 (option spaces) float4 in
    let descs= points |> List.map (fun (p1, p2, p3, p4)->
      {
        ctrl= {x=p1; y=p2};
        end'= {x=p3; y=p4};
      })
    in
    return @@ Cmd_Q descs
  let cmd_q=
    let* _= tag_q in
    let* points= sepStartBy1 (option spaces) float4 in
    let descs= points |> List.map (fun (p1, p2, p3, p4)->
      { ctrl={x=p1; y=p2};
        end'={x=p3; y=p4};
      })
    in
    return @@ Cmd_q descs
  let cmd_T=
    let* _= tag_T in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_T points
  let cmd_t=
    let* _= tag_t in
    let* points= sepStartBy1 (option spaces) point in
    return @@ Cmd_t points

  let cmd_A=
    let* _= tag_A in
    let* descs= sepStartBy1 (option spaces) arc_desc in
    return @@ Cmd_A descs

  let cmd_a=
    let* _= tag_a in
    let* descs= sepStartBy1 (option spaces) arc_desc in
    return @@ Cmd_a descs

  let cmd_Z=let* _= tag_Z in return Cmd_Z
  let cmd_z= let* _=tag_z in return Cmd_z

  let path=
    sepStartBy1 spaces (
      cmd_M <|> cmd_m
      <|> cmd_L <|> cmd_l <|> cmd_H <|> cmd_h <|> cmd_V <|> cmd_v
      <|> cmd_C <|> cmd_c <|> cmd_S <|> cmd_s
      <|> cmd_Q <|> cmd_q <|> cmd_T <|> cmd_t
      <|> cmd_A <|> cmd_a
      <|> cmd_Z <|> cmd_z)
end

let sub_of_parser_commands (commands:Parser.command list)=
  let[@tail_mod_cons] rec of_parser_commands (p_commands:Parser.command list)=
    match p_commands with
    | []-> [], []
    | hd::tl-> match hd with
      | Cmd_M _-> [], p_commands
      | Cmd_m _-> [], p_commands
      | Cmd_L points->
        (match points with
        | point::points->
          let commands, remaining= of_parser_commands (Cmd_L points :: tl) in
          Cmd_L point :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_l points->
        (match points with
        | point::points->
          let commands, remaining= of_parser_commands (Cmd_l points :: tl) in
          Cmd_l point :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_H floats->
        (match floats with
        | float::floats->
          let commands, remaining= of_parser_commands (Cmd_H floats :: tl) in
          Cmd_H float :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_h floats->
        (match floats with
        | float::floats->
          let commands, remaining= of_parser_commands (Cmd_h floats :: tl) in
          Cmd_h float :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_V floats->
        (match floats with
        | float::floats->
          let commands, remaining= of_parser_commands (Cmd_V floats :: tl) in
          Cmd_V float :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_v floats->
        (match floats with
        | float::floats->
          let commands, remaining= of_parser_commands (Cmd_v floats :: tl) in
          Cmd_v float :: commands, remaining
        | []-> of_parser_commands tl)

      | Cmd_C descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_C descs :: tl) in
          Cmd_C desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_c descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_c descs :: tl) in
          Cmd_c desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_S descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_S descs :: tl) in
          Cmd_S desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_s descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_s descs :: tl) in
          Cmd_s desc :: commands, remaining
        | []-> of_parser_commands tl)

      | Cmd_Q descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_Q descs :: tl) in
          Cmd_Q desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_q descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_q descs :: tl) in
          Cmd_q desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_T points->
        (match points with
        | point::points->
          let commands, remaining= of_parser_commands (Cmd_T points :: tl) in
          Cmd_T { end'= point } :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_t points->
        (match points with
        | point::points->
          let commands, remaining= of_parser_commands (Cmd_t points :: tl) in
          Cmd_t { end'= point } :: commands, remaining
        | []-> of_parser_commands tl)

      | Cmd_A descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_A descs :: tl) in
          Cmd_A desc :: commands, remaining
        | []-> of_parser_commands tl)
      | Cmd_a descs->
        (match descs with
        | desc::descs->
          let commands, remaining= of_parser_commands (Cmd_a descs :: tl) in
          Cmd_a desc :: commands, remaining
        | []-> of_parser_commands tl)

      | Cmd_Z | Cmd_z-> [], tl
  in
  match commands with
  | Cmd_M (point::points)::tl->
    (try
      let start= Absolute point in
      let (segments, remaining)= of_parser_commands ((Cmd_L points)::tl) in
      Some { start; segments }, remaining
    with _ -> None, commands)
  | Cmd_m (point::points)::tl->
    (try
      let start= Relative point in
      let segments, remaining= of_parser_commands ((Cmd_l points)::tl) in
      Some { start; segments }, remaining
    with _-> None, commands)
  | _-> None, commands

let rec of_parser_commands (commands:Parser.command list)=
  match sub_of_parser_commands commands with
  | Some cmd, remaining->
    let cmds, remaining= of_parser_commands remaining in
    (cmd::cmds), remaining
  | None, remaining-> [], remaining

let of_string str=
  match Utils.MiniParsec.parse_string Parser.path str with
  | Ok (commands, _)->
    let t, _= of_parser_commands commands in
    Some t
  | Error _-> None

