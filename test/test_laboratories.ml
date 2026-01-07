open Core
open Hardcaml
open Adventoffpga
open Stdio

module Simulator = Cyclesim.With_interface(Laboratories.I)(Laboratories.O)

let example1 = {|
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
|}

let bits_of_line line =
  Array.map (String.to_array (String.strip line)) ~f:(function
    |'^' | 'S' -> Bits.vdd
    | _ -> Bits.gnd)
  |> Bits.of_array

let line_of_bits ~line bits =
  String.mapi line ~f:(fun i c ->
    match Bits.bit bits i |> Bits.is_vdd with
    | true when Char.(c <> 'S') -> '|'
    | _ -> c)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  let sim = Simulator.create Laboratories.create in
  (* let filename = "/tmp/waves.vcd" in *)
  (* let oc = Out_channel.create filename in *)
  (* let sim = Vcd.wrap oc sim in *)
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  (* Reset the design *)
  inputs.enable := Bits.gnd;
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.enable := Bits.vdd;
  String.split_lines example1
  |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  |> List.iteri ~f:(fun i line ->
      inputs.set := Bits.of_bool (i = 0);
      inputs.data := bits_of_line line;
      cycle ();
      print_endline (line_of_bits ~line !(outputs.line)));
  let result = Bits.to_int !(outputs.out) in
  Format.printf "result=%u@." result;
  [%expect {|
    .......S.......
    .......|.......
    ......|^|......
    ......|.|......
    .....|^|^|.....
    .....|.|.|.....
    ....|^|^|^|....
    ....|.|.|.|....
    ...|^|^|||^|...
    ...|.|.|||.|...
    ..|^|^|||^|^|..
    ..|.|.|||.|.|..
    .|^|||^||.||^|.
    .|.|||.||.||.|.
    |^|^|^|^|^|||^|
    |.|.|.|.|.|||.|
    result=21
    |}]
