(*
 * test.ml
 * -----------
 * Copyright : (c) 2023 - 2023, smaji.org
 * Copyright : (c) 2023 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : GPL2
 *
 * This file is a part of Smaji_glyph_outline.
 *)

open Smaji_glyph_outline


let%expect_test "move"=
  (match Svg.Path.of_string "M12,23-3,1m 0.2\n, \n0.3L 20 , 20" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "
    Absolute 12.0,23.0; L -3.0,1.0
    Relative 0.2,0.3; L 20.0,20.0"]


let%expect_test "move"=
  (match Svg.Path.of_string "m12,23-3,1 2,3L20 , 20" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Relative 12.0,23.0; l -3.0,1.0; l 2.0,3.0; L 20.0,20.0"]


let%expect_test "Ccurve"=
  (match Svg.Path.of_string "M20,20C1,2,3,4,5,6-1,2,3,4,5,6\n1,2,3,4,5,6" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Absolute 20.0,20.0; C {ctrl1: 1.0,2.0; ctrl2: 3.0,4.0; end: 5.0,6.0}; C {ctrl1: -1.0,2.0; ctrl2: 3.0,4.0; end: 5.0,6.0}; C {ctrl1: 1.0,2.0; ctrl2: 3.0,4.0; end: 5.0,6.0}"]

let%expect_test "SCcurve"=
  (match Svg.Path.of_string "M20,20S3,4,5,6-3,4,5,6\n3,4,5,6" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Absolute 20.0,20.0; S {ctrl2: 3.0,4.0; end: 5.0,6.0}; S {ctrl2: -3.0,4.0; end: 5.0,6.0}; S {ctrl2: 3.0,4.0; end: 5.0,6.0}"]


let%expect_test "Qcurve"=
  (match Svg.Path.of_string "M20,20Q1,2,3,4-1,2,3,4q1,2,3,4" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Absolute 20.0,20.0; Q {ctrl: 1.0,2.0; end: 3.0,4.0}; Q {ctrl: -1.0,2.0; end: 3.0,4.0}; q {ctrl: 1.0,2.0; end: 3.0,4.0}"]

let%expect_test "SQcurve"=
  (match Svg.Path.of_string "M20,20T1,2-1,2t1,2" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Absolute 20.0,20.0; T 1.0,2.0; T -1.0,2.0; t 1.0,2.0"]


let%expect_test "arc"=
  (match Svg.Path.of_string "M20,20A1,2,0,0,1,3,4 1,2,0,1,0,3,4a1,2,0,1,1,3,4" with
  | Some path->
    let path_str= path |> List.map Svg.Path.sub_to_string_hum |> String.concat "\n" in
    print_endline path_str
  | None-> ());
  [%expect "Absolute 20.0,20.0; A {rx: 1.0; ry: 2.0; angle: 0.0; large_arc: false; sweep: true; end: 3.0,4.0}; A {rx: 1.0; ry: 2.0; angle: 0.0; large_arc: true; sweep: false; end: 3.0,4.0}; a {rx: 1.0; ry: 2.0; angle: 0.0; large_arc: true; sweep: true; end: 3.0,4.0}"]


let%expect_test "viewBox"=
  (match Svg.ViewBox.of_string " 1 2, 3  ,  4 \n " with
  | Some viewBox-> Svg.ViewBox.to_string_hum viewBox |> print_endline
  | None-> print_endline "");
  [%expect "{min_x: 1.0; min_y: 2.0; width: 3.0; height: 4.0}"]

let viewBox= Svg.ViewBox.{ min_x= 1.0; min_y= 2.0; width= 4.0; height= 3.0 }
and path1=
  Svg.Path.{
    start= Absolute (1.0, 2.);
    segments= [
      Cmd_l (3.0, 4.);
      Cmd_v 5.0;
      Cmd_h 6.0;
      ];
  }
and path2=
  Svg.Path.{
    start= Relative (1.0, 2.);
    segments= [
      Cmd_l (3.0, 4.);
      Cmd_v 5.0;
      Cmd_h 6.0;
      Cmd_t (7.0, 8.);
      Cmd_C {
        ctrl1= (1.0, 2.);
        ctrl2= (3.0, 4.);
        end'= (5.0, 6.);
        };
      Cmd_a {
        rx= 1.0; ry= 2.0;
        angle= 0.0;
        large_arc= true;
        sweep= false;
        end'= (3.0, 4.);
        };
      ];
  }
let paths_individual= [
  [path1];
  [path2];
  ]
and paths_continuous= [
  [path1; path2];
  ]

let svg_individual= Svg.{ viewBox; paths= paths_individual }
let svg_continuous= Svg.{ viewBox; paths= paths_continuous }

let%expect_test "svg_to_string_individual"=
  Svg.to_string svg_individual |> print_endline;
  [%expect "
    <svg viewBox=\"1.0,2.0 4.0,3.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z\"
      />
      <path d=\"
        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 1.0,2.0,3.0,4.0,5.0,6.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "svg_to_string_continuous"=
  Svg.to_string svg_continuous |> print_endline;
  [%expect "
    <svg viewBox=\"1.0,2.0 4.0,3.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z

        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 1.0,2.0,3.0,4.0,5.0,6.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "svg_reset_viewBox_1"=
  let svg= svg_individual |> Svg.Adjust.reset_viewBox in
  Svg.to_string svg |> print_endline;
  [%expect "
    <svg viewBox=\"0.0,0.0 4.0,3.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 0.0,0.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z\"
      />
      <path d=\"
        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 0.0,0.0,2.0,2.0,4.0,4.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "svg_reset_viewBox_2"=
  let viewBox= { viewBox with
    min_x= -. viewBox.min_x;
    min_y= -. viewBox.min_y }
  in
  let svg= Svg.{ viewBox; paths= paths_individual } |> Svg.Adjust.reset_viewBox in
  Svg.to_string svg |> print_endline;
  [%expect "
    <svg viewBox=\"0.0,0.0 4.0,3.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 2.0,4.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z\"
      />
      <path d=\"
        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 2.0,4.0,4.0,6.0,6.0,8.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "get_path_frame individual"=
  paths_individual
    |> Svg.Path.get_frame_paths
    |> Option.iter (fun frame-> frame
      |> Svg.Path.string_of_frame
      |> print_endline);
  [%expect "{ px= 17.0; nx= 1.0; py= 19.0; ny= 2.0 }"]

let%expect_test "get_path_frame continuous"=
  paths_continuous
    |> Svg.Path.get_frame_paths
    |> Option.iter (fun frame-> frame
      |> Svg.Path.string_of_frame
      |> print_endline);
  [%expect "{ px= 27.0; nx= 1.0; py= 30.0; ny= 2.0 }"]

let%expect_test "fit_frame_individual"=
  Svg.Adjust.fit_frame svg_individual |> Svg.to_string |> print_endline;
  [%expect "
    <svg viewBox=\"0.0,0.0 16.0,17.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 0.0,0.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z\"
      />
      <path d=\"
        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 0.0,0.0,2.0,2.0,4.0,4.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "fit_frame_continuous"=
  Svg.Adjust.fit_frame svg_continuous |> Svg.to_string |> print_endline;
  [%expect "
    <svg viewBox=\"0.0,0.0 26.0,28.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 0.0,0.0
        l 3.0,4.0
        v 5.0
        h 6.0
        Z

        m 1.0,2.0
        l 3.0,4.0
        v 5.0
        h 6.0
        t 7.0,8.0
        C 0.0,0.0,2.0,2.0,4.0,4.0
        a 1.0,2.0,0.0,1,0,3.0,4.0
        Z\"
      />
    </svg>"]

let%expect_test "load_file"=
  (match Svg.load_file "a.svg" with
  | Some svg-> svg |> Svg.to_string |> print_endline
  | None-> print_endline "");
  [%expect "
    <svg viewBox=\"45.0,-33.8 150.0,150.0\" xmlns=\"http://www.w3.org/2000/svg\">
      <path d=\"
        M 151.3,99.8
        c -18.6,-0.1,-33.9,-7.4,-46.1,-21.9
        c -9.4,-11.3,-14.1,-24.1,-14.1,-38.2
        c 0.0,-18.8,7.3,-34.2,21.9,-46.4
        c 11.2,-9.3,24.0,-14.0,38.3,-14.1
        v 5.3
        h -0.1
        c -15.0,0.0,-27.2,5.0,-36.6,14.9
        c -9.6,10.2,-14.4,23.6,-14.4,40.3
        c 0.0,16.4,5.0,29.8,14.9,40.3
        c 9.7,9.8,21.8,14.6,36.3,14.6
        V 99.8
        Z\"
      />
    </svg>"]

let%expect_test "file_fit_frame"=
  (match Svg.load_file "a.svg" with
  | Some svg->
    svg |> Svg.Adjust.fit_frame |> Svg.to_string |> print_endline
  | None-> print_endline "");
  [%expect {|
    <svg viewBox="0.0,0.0 60.3,120.6" xmlns="http://www.w3.org/2000/svg">
      <path d="
        M 60.2,120.6
        c -18.6,-0.1,-33.9,-7.4,-46.1,-21.9
        c -9.4,-11.3,-14.1,-24.1,-14.1,-38.2
        c 0.0,-18.8,7.3,-34.2,21.9,-46.4
        c 11.2,-9.3,24.0,-14.0,38.3,-14.1
        v 5.3
        h -0.1
        c -15.0,0.0,-27.2,5.0,-36.6,14.9
        c -9.6,10.2,-14.4,23.6,-14.4,40.3
        c 0.0,16.4,5.0,29.8,14.9,40.3
        c 9.7,9.8,21.8,14.6,36.3,14.6
        V 120.6
        Z"
      />
    </svg> |}]

