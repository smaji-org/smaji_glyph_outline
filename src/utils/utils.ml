(*
 * utils.ml
 * -----------
 * Copyright : (c) 2023 - 2025, smaji.org
 * Copyright : (c) 2023 - 2025, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_path.
 *)

open Result

let string_of_float f=
  let str= string_of_float f in
  if String.ends_with ~suffix:"." str then str ^ "0" else str

let xml_attr_opt name attrs=
  try Some (Ezxmlm.get_attr name attrs) with _-> None

let xml_member_opt name nodes=
  try
    Some (Ezxmlm.member name nodes)
  with
    Ezxmlm.Tag_not_found _-> None

let int_of_hex str= int_of_string ("0x" ^ str)

module MiniParsec = struct
  open Printf

  type pos= {
    cnum: int;
    line: int;
    bol: int;
  }

  type state= {
    data: string;
    maxlen: int;
    pos: pos
  }

  let initState data= {
    data;
    maxlen= String.length data;
    pos= {
      cnum= 0;
      line= 1;
      bol= 0;
      };
  }

  type error= pos * string

  type 'a reply= (('a * state), error) result

  type 'a parser= state -> 'a reply
  type 'a t= 'a parser


  let string_of_pos pos= sprintf "line %d, characters %d"
    pos.line (pos.cnum - pos.bol)

  let string_of_pos_full pos= sprintf "offset %d, line %d, characters %d"
    pos.cnum pos.line (pos.cnum - pos.bol)

  (* parser generator *)

  let any= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data state.pos.cnum in
      let pos= { pos with cnum= pos.cnum + 1 } in
      (Ok (found, { state with pos }))
    else
      (Error (state.pos, "out of bounds"))

  let char c= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = c then
        let pos= { pos with cnum= pos.cnum + 1 } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%c\" expected but \"%c\" found" c found)
    else
      (Error (state.pos, "out of bounds"))

  let string str= fun state->
    let pos= state.pos in
    let len= String.length str in
    if state.maxlen - pos.cnum >= len then
      let found= String.sub state.data pos.cnum len in
      if found = str then
        let pos= { pos with cnum= pos.cnum + len } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%s\" expected but \"%s\" found" str found)
    else
      (Error (state.pos, "out of bounds"))


  let satisfy test= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if test found then
        let pos= { pos with cnum= pos.cnum + 1 } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%c\" isn't satisfied" found)
    else
      (Error (state.pos, "out of bounds"))

  (* combinator *)
  let fail msg= fun state-> Error (state.pos, msg)

  let return v= fun state-> Ok (v, state)

  let bind (p: 'a parser) (f: 'a -> 'b parser)= fun state->
    let result= p state in
    match result with
    | Error e-> Error e
    | Ok (v,state)-> f v state

  let (>>=)= bind
  let (>>) p1 p2= p1 >>= fun _ -> p2
  let (<<) p1 p2= p1 >>= fun x-> p2 >> return x
  let (|>>) p f= p >>= fun v-> return (f v)
  let (>>$) p v= p >> return v

  let (<|>) (p1:'a parser) (p2:'a parser)= fun state->
    let result= p1 state in
    match result with
    | Error _-> p2 state
    | Ok _-> result

  let between left right p= left >> p << right

  let many p=
    let rec parser s=
      (((p |>> fun v-> Some v) <|> return None) >>= (function
        | Some v-> parser |>> (fun r-> v :: r)
        | None-> return []))
        s
    in parser

  let many1 p=
    p >>= fun v-> many p |>> fun l-> v :: l

  let rec times num p s=
    if num > 0 then
      (p >>= (fun v-> times (num-1) p |>> (fun r-> v::r))) s
    else
      (return []) s

  let sepStartBy sep p= many (sep >> p)

  let sepStartBy1 sep p= many1 (sep >> p)

  let sepEndBy sep p= many (p << sep)

  let sepEndBy1 sep p= many1 (p << sep)

  let sepBy1 sep p=
    p >>= fun head->
    sepStartBy sep p >>= fun body->
    return (head :: body)

  let sepBy sep p= sepBy1 sep p <|> return []

  let opt default p=
    p <|> return default

  let option p= p |>> (fun v-> Some v) <|> return None

  let lookAhead p= fun state->
    let reply= p state in
    match reply with
    | Ok (r, newState)-> Ok (r, state)
    | Error _-> reply
    [@@ocaml.warning "-27"]

  let followedBy p msg= fun state->
    let reply= p state in
    match reply with
    | Ok _-> Ok ((), state)
    | Error _-> Error (state.pos, msg)

  let notFollowedBy p msg= fun state->
    let reply= p state in
    match reply with
    | Ok _-> Error (state.pos, msg)
    | Error _-> Ok ((), state)

  (* parser *)
  let eof state=
    if state.pos.cnum >= state.maxlen
    then Ok ((), state)
    else Error (state.pos, "not eof")

  let newline_lf state=
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = '\n' then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (String.make 1 found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-lf expected but \"%c\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_cr state=
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = '\r' then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (String.make 1 found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-cr expected but \"%c\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_crlf state=
    let pos= state.pos in
    if pos.cnum + 2 <= state.maxlen then
      let found= String.sub state.data pos.cnum 2 in
      if found = "\r\n" then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-crlf expected but \"%s\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_lfcr state=
    let pos= state.pos in
    if pos.cnum + 2 <= state.maxlen then
      let found= String.sub state.data pos.cnum 2 in
      if found = "\n\r" then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-lfcr expected but \"%s\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline= newline_crlf <|> newline_lfcr
    <|> newline_lf <|> newline_cr


  let int8= any |>> int_of_char

  let int16= any >>= fun l-> any
    |>> fun h-> int_of_char h lsl 8 + int_of_char l
  let int16_net= any >>= fun h-> any
    |>> fun l-> int_of_char h lsl 8 + int_of_char l

  let int32= int16 >>= fun l-> int16
    |>> fun h-> Int32.(add (shift_left (of_int h) 16) (of_int l))
  let int32_net= int16_net >>= fun h-> int16_net
    |>> fun l-> Int32.(add (shift_left (of_int h) 16) (of_int l))

  let int64= int32 >>= fun l-> int32
    |>> fun h-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))
  let int64_net= int32_net >>= fun h-> int32_net
    |>> fun l-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))

  let num_dec= satisfy (fun c->
    '0' <= c && c <= '9')

  let num_bin= satisfy (fun c->
    c = '0' || c = '1')

  let num_oct= satisfy (fun c->
    '0' <= c && c <= '7')

  let num_hex= satisfy (fun c->
    '0' <= c && c <= '9'
    || 'a' <= c && c <= 'f'
    || 'A' <= c && c <= 'F')

  let lowercase= satisfy (fun c->
    'a' <= c && c <= 'z')

  let uppercase= satisfy (fun c->
    'A' <= c && c <= 'Z')

  (* start parsing *)
  let parse_string parser str= parser (initState str)
end

module Elt = struct
  type length=
    | Finite of int
    | Infinite

  type 'a t= {
    value: 'a;
    mutable left: 'a t option;
    mutable right: 'a t option;
  }

  let left elt=
    elt.left

  let right elt=
    elt.right

  let init ~f n=
    if n < 1 then invalid_arg "length should be at least 1" else
    let init= { value= f 0; left= None; right= None } in
    let prev= ref init in
    for i= 1 to n-1 do
      let next= { value= f i; left= Some !prev; right= None } in
      !prev.right <- Some next;
      prev:= next;
    done;
    init

  let length_left elt=
    let anchor= elt in
    let rec length acc elt=
      match elt.left with
      | Some elt->
        if elt == anchor then
          Infinite
        else
          length (acc+1) elt
      | None-> Finite acc
    in
    length 1 elt

  let length_right elt=
    let anchor= elt in
    let rec length acc elt=
      match elt.right with
      | Some elt->
        if elt == anchor then
          Infinite
        else
          length (acc+1) elt
      | None-> Finite acc
    in
    length 1 elt

  let length elt=
    match (length_left elt, length_right elt) with
    | (Finite left, Finite right)-> Finite (left + right - 1)
    | _-> Infinite

  let insert_left elt value=
    let left= left elt in
    let new_elt= {
      value;
      left= left;
      right= Some elt;
    }
    in
    (match left with
    | Some left-> left.right <- Some new_elt
    | None-> ());
    elt.left <- Some new_elt

  let insert_right elt value=
    let right= right elt in
    let new_elt= {
      value;
      left= Some elt;
      right= right;
    }
    in
    elt.right <- Some new_elt;
    match right with
    | Some right-> right.left <- Some new_elt
    | None-> ()

  let remove elt=
    let left= elt.left
    and right= elt.right in
    (match left with
    | Some left-> left.right <- right
    | None-> ());
    (match right with
    | Some right-> right.left <- left
    | None-> ())

  let rec iter_left ~f elt=
    f elt.value;
    Option.iter (iter_left ~f) elt.left

  let rec iter_right ~f elt=
    f elt.value;
    Option.iter (iter_right ~f) elt.right

  let iter ~f elt=
    iter_left ~f elt;
    Option.iter (iter_right ~f) elt.right

  let map_left ~f elt=
    let rec map_left ~f elt_right elt_from=
      let elt_new= {
        value= f elt_from.value;
        left= None;
        right= Some elt_right;
      }
      in
      elt_right.left <- Some elt_new;
      Option.iter (map_left ~f elt_new) elt_from.left
    in
    let init= {
      value= f elt.value;
      left= None;
      right= None;
    } in
    Option.iter (map_left ~f init) elt.left;
    init

  let map_right ~f elt=
    let rec map_right ~f elt_left elt_from=
      let elt_new= {
        value= f elt_from.value;
        left= Some elt_left;
        right= None;
      }
      in
      elt_left.right <- Some elt_new;
      Option.iter (map_right ~f elt_new) elt_from.right
    in
    let init= {
      value= f elt.value;
      left= None;
      right= None;
    } in
    Option.iter (map_right ~f init) elt.right;
    init

  let map ~f elt=
    let elt_left= map_left ~f elt in
    let elt_right= Option.map (map_right ~f) elt.right in
    elt_left.right <- elt_right;
    Option.iter (fun elt_right-> elt_right.left <- Some elt_left) elt_right;
    elt_left

  let rec fold_left ~f ~init elt=
    let init= f init elt.value in
    match elt.left with
    | Some elt-> fold_left ~f ~init elt
    | None-> init

  let rec fold_right ~f ~init elt=
    let init= f init elt.value in
    match elt.right with
    | Some elt-> fold_right ~f ~init elt
    | None-> init

  let fold ~f ~init elt=
    let acc= fold_left ~f ~init elt in
    elt.right
      |> Option.map (fold_right ~f ~init:acc)
      |> Option.value ~default:acc
end

module Dlist = struct
  type 'a t= {
    mutable head: 'a Elt.t option;
    mutable tail: 'a Elt.t option;
    mutable length: int;
  }

  let head t= t.head
  let tail t= t.tail

  let init ~f n=
    let open Elt in
    if n < 0 then
      invalid_arg "length should be at least 0"
    else if n = 0 then
      {
        head= None;
        tail= None;
        length= n;
      }
    else
      let init= { value= f 0; left= None; right= None } in
      let prev= ref init in
      for i= 1 to n-1 do
        let next= { value= f i; left= Some !prev; right= None } in
        !prev.right <- Some next;
        prev:= next;
      done;
      {
        head= Some init;
        tail= Some !prev;
        length= n;
      }

  let of_list list=
    let rec of_list len prev list=
      match list with
      | []-> len, prev
      | hd::tl->
        let new_elt=
          Elt.{
            value= hd;
            left= Some prev;
            right= None;
          }
        in
        prev.right <- Some new_elt;
        of_list (len+1) new_elt tl
    in
    match list with
    | []-> {
        head= None;
        tail= None;
        length= 0;
      }
    | hd::tl->
      let head= Elt.{
        value= hd;
        left= None;
        right= None;
      }
      in
      let (length, tail)= of_list 1 head tl in
      {
        head= Some head;
        tail= Some tail;
        length;
      }

  let to_list dlist=
    let[@tail_mod_cons] rec to_list elt=
      match elt.Elt.right with
      | None-> [elt.value]
      | Some next-> elt.value :: to_list next
    in
    match dlist.head with
    | None-> []
    | Some head-> to_list head

  let length t= t.length

  let insert_elt_left t elt value=
    Elt.insert_left elt value;
    Option.iter
      (fun head-> if head == elt then t.head <- head.left)
      t.head

  let insert_elt_right t elt value=
    Elt.insert_right elt value;
    Option.iter
      (fun tail-> if tail == elt then t.tail <- tail.right)
      t.head

  let insert_first t value=
    if length t = 0 then
      let elt= Elt.{
        value;
        left= None;
        right= None;
      }
      in
      {
        head= Some elt;
        tail= Some elt;
        length= 1;
      }
    else
      let elt= Elt.{
        value;
        left= None;
        right= t.head;
      }
      in
      Option.iter (fun head-> head.Elt.left <- Some elt) t.head;
      {
        t with
        head= Some elt;
        length= t.length+1;
      }

  let insert_last t value=
    if length t = 0 then
      let elt= Elt.{
        value;
        left= None;
        right= None;
      }
      in
      {
        head= Some elt;
        tail= Some elt;
        length= 1;
      }
    else
      let elt= Elt.{
        value;
        left= t.tail;
        right= None;
      }
      in
      Option.iter (fun tail-> tail.Elt.right <- Some elt) t.tail;
      {
        t with
        tail= Some elt;
        length= t.length+1;
      }

  let remove t elt=
    if t.length > 0 then begin
      Option.iter
        (fun head->
          if head == elt then
            t.head <- head.Elt.right)
        t.head;
      Option.iter
        (fun tail->
          if tail == elt then
            t.tail <- tail.Elt.left)
        t.tail;
      Elt.remove elt;
    end
    
  let iter ~f t=
    Option.iter (Elt.iter_right ~f) t.head

  let iter_rev ~f t=
    Option.iter (Elt.iter_left ~f) t.tail

  let map ~f t=
    let rec map_right ~f elt_left elt_from=
      let elt_new= Elt.{
        value= f elt_from.value;
        left= Some elt_left;
        right= None;
      }
      in
      elt_left.right <- Some elt_new;
      match elt_from.right with
      | Some right-> (map_right ~f elt_new right)
      | None-> elt_new
    in
    match t.head with
    | None->
      {
        length=0;
        head= None;
        tail= None;
      }
    | Some elt->
      let init= Elt.{
        value= f elt.value;
        left= None;
        right= None;
      } in
      let tail= Option.map (map_right ~f init) elt.right in
      {
        length= t.length;
        head= Some init;
        tail;
      }

  let fold ~f ~init t=
    match t.head with
    | Some head-> Elt.fold_right ~f ~init head
    | None-> init
end

module Circle = struct
  type 'a t= {
    mutable size: int;
    mutable entry: 'a elt option;
  }
  and 'a elt= {
    value: 'a;
    circle: 'a t;
    mutable left: 'a elt;
    mutable right: 'a elt;
  }

  let left elt= elt.left

  let right elt= elt.right

  let size t= t.size

  let entry t= t.entry

  let set_entry t elt=
    if elt.circle == t then
      (t.entry <- Some elt;
      Ok ())
    else
      Error (Invalid_argument "the element does not belong to the circle")

  let init ~f n=
    if n < 0 then invalid_arg "length should be at least 0"
    else if n = 0 then
      {
        size= 0;
        entry= None;
      }
    else
    let rec circle= {
      size= n;
      entry= Some entry;
    }
    and entry= { value= f 0; circle; left= entry; right= entry } in
    let prev= ref entry in
    for i= 1 to n-1 do
      let rec next= { value= f i; circle; left= !prev; right= next } in
      !prev.right <- next;
      prev:= next;
    done;
    !prev.right <- entry;
    entry.left <- !prev;
    circle

  let of_list list=
    let rec of_list circle len prev list=
      match list with
      | []-> len, prev
      | hd::tl->
        let rec new_elt=
          {
            value= hd;
            circle;
            left= prev;
            right= new_elt;
          }
        in
        prev.right <- new_elt;
        of_list circle (len+1) new_elt tl
    in
    match list with
    | []-> {
        entry= None;
        size= 0;
      }
    | hd::tl->
      let rec circle= {
        entry= Some entry;
        size= 0;
      }
      and entry= {
        value= hd;
        circle;
        left= entry;
        right= entry;
      }
      in
      let (size, tail)= of_list circle 1 entry tl in
      circle.size <- size;
      entry.left <- tail;
      tail.right <- entry;
      circle

  let to_list circle=
    match circle.entry with
    | None-> []
    | Some entry->
      let[@tail_mod_cons] rec to_list elt=
        if elt.right == entry then
          [elt.value]
        else
          elt.value :: to_list elt.right
      in
      to_list entry

  let insert_left elt value=
    let left= elt.left in
    let new_elt= {
      value;
      circle= elt.circle;
      left= elt.left;
      right= elt;
    }
    in
    left.right <- new_elt;
    elt.left <- new_elt;
    elt.circle.size <- elt.circle.size + 1

  let insert_right elt value=
    let right= elt.right in
    let new_elt= {
      value;
      circle= elt.circle;
      left= elt;
      right= elt.right;
    }
    in
    right.left <- new_elt;
    elt.right <- new_elt;
    elt.circle.size <- elt.circle.size + 1

  let remove elt=
    let circle= elt.circle in
    if circle.size = 1 then (
      circle.entry <- None;
      circle.size <- 0;
    ) else if circle.size > 1 then (
      let left= elt.left
      and right= elt.right in
      left.right <- right;
      right.left <- left;
      circle.entry
        |> Option.iter (fun entry->
          if entry == elt then
            circle.entry <- Some right);
      circle.size <- circle.size - 1;
    )

  let iter_left ~f elt=
    let entry= elt in
    let rec iter_left ~f elt=
      f elt.value;
      if elt.left != entry then iter_left ~f elt.left
    in
    iter_left ~f elt

  let iter_right ~f elt=
    let entry= elt in
    let rec iter_right ~f elt=
      f elt.value;
      if elt.right != entry then iter_right ~f elt.right
    in
    iter_right ~f elt

  let map ~f circle=
    match circle.entry with
    | Some elt->
      let entry_original= elt in
      let rec map ~f circle elt_left elt_from=
        if elt_from == entry_original then
          elt_left
        else
          let rec elt_new= {
            value= f elt_from.value;
            circle;
            left= elt_left;
            right= elt_new;
          }
          in
          elt_left.right <- elt_new;
          map ~f circle elt_new elt_from.right
      in
        let rec circle= {
          entry= Some entry;
          size= elt.circle.size;
        }
        and entry= {
          value= f elt.value;
          circle;
          left= entry;
          right= entry;
        } in
        let last= elt.right |> map ~f circle entry in
        entry.left <- last;
        last.right <- entry;
        circle
    | None->
      {
        entry= None;
        size= 0;
      }

  let fold_left ~f ~init t=
    match t.entry with
    | Some entry->
      let rec fold_left ~f ~acc elt=
        let acc= f acc elt.value in
        if elt.left == entry then
          acc
        else
          fold_left ~f ~acc elt.left
      in
      fold_left ~f ~acc:init entry
    | None-> init

  let fold_right ~f ~init t=
    match t.entry with
    | Some entry->
      let rec fold_right ~f ~acc elt=
        let acc= f acc elt.value in
        if elt.right == entry then
          acc
        else
          fold_right ~f ~acc elt.right
      in
      fold_right ~f ~acc:init entry
    | None-> init
end

