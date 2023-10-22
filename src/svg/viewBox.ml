open Utils

type t= {
  min_x: float;
  min_y: float;
  width: float;
  height: float;
}

module Parser = struct
  open Utils.MiniParsec

  let ( let* )= bind

  let string_of_cl cl= String.concat "" (List.map (String.make 1) cl)

  let space= char ' ' <|> char '\t' <|> (newline |>> (fun _-> '\n'))

  let spaces= many space

  let spaces1= many1 space

  let number_sep= many1 (space <|> char ',')

  let float1=
    let* neg= option (char '-') in
    let* integer= many1 num_dec << option (char '.') in
    let* fractional= option  (many1 num_dec) in
    let neg= Option.is_some neg in
    let integer= integer |> string_of_cl |> float_of_string in
    let fractional= match fractional with
      | None-> 0.
      | Some cl-> "." ^ (string_of_cl cl) |> float_of_string
    in
    let num= integer +. fractional in
    let result= if neg then -. num else num in
    return result

  let float4=
    let* point1= float1 in
    let* _= number_sep in
    let* point2= float1 in
    let* _= number_sep in
    let* point3= float1 in
    let* _= number_sep in
    let* point4= float1 in
    return (point1, point2, point3, point4)

  let viewBox=
    let* _= spaces in
    let* (min_x, min_y, width, height)= float4 in
    return {
      min_x;
      min_y;
      width;
      height;
    }
end

let of_string str=
  match Utils.MiniParsec.parse_string Parser.viewBox str with
  | Ok (viewBox, _)-> Some viewBox
  | Error _-> None

let to_string_hum t=
  Printf.sprintf "{min_x: %s; min_y: %s; width: %s; height: %s}"
    (string_of_float t.min_x)
    (string_of_float t.min_y)
    (string_of_float t.width)
    (string_of_float t.height)

let to_string_svg t=
  Printf.sprintf "%s,%s %s,%s"
    (string_of_float t.min_x)
    (string_of_float t.min_y)
    (string_of_float t.width)
    (string_of_float t.height)

