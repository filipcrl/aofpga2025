open Core
open Hardcaml
open Adventoffpga
module Simulator = Cyclesim.With_interface (Laboratories.I) (Laboratories.O)

let ceil_div = Laboratories.ceil_div

let aocexample = {|
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

let twolines = {|
..S..
..^..
.^...
....^
|}

let strip_and_filter_lines str =
  String.split_lines str
  |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))

let bits_of_line line =
  Array.map (String.to_array (String.strip line)) ~f:(function
    |'^' | 'S' -> Bits.vdd
    | _ -> Bits.gnd)
  |> Array.rev |> Bits.of_array

let line_of_bits ~line bits =
  String.mapi line ~f:(fun i c ->
    match Bits.bit bits i |> Bits.is_vdd with
    | true when Char.(c <> 'S') -> '|'
    | _ -> c)

let bits_of_word word = Bits.of_int ~width:Laboratories.word_width word

let words_of_line ~word_w line =
  let open Bits in
  let bits = bits_of_line line in
  Format.printf "line=%a @." Bits.pp bits;
  let num_of_bits = ceil_div (width bits) word_w * word_w in
  let extra = num_of_bits - (width bits) in
  Bits.split_msb ~part_width:word_w (sll (uresize bits num_of_bits) extra)

let%expect_test "Short input" =
  let scope = Scope.create ~flatten_design:false () in
  let sim = Simulator.create ~config:Cyclesim.Config.trace_all (Laboratories.create scope) in
  let filename = "/tmp/short_input.vcd" in
  let oc = Out_channel.create filename in
  let sim = Vcd.wrap oc sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let bits = ref Bits.empty in
  let cycle_ = ref 0 in
  let cycle () =
    cycle_ := !cycle_ + 1;
    Cyclesim.cycle sim;
    Format.printf "beams_valid [%d] %a @." !cycle_ Bits.pp !(outputs.beams_valid);
    if Bits.is_vdd !(outputs.beams_valid) then
      Format.printf "received [%d] %a @." !cycle_ Bits.pp !(outputs.beams_next);
      bits := Bits.concat_msb_e [(!bits); !(outputs.beams_next)];
  in
  inputs.valid := Bits.gnd;
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.vdd;

  List.iter (strip_and_filter_lines twolines) ~f:(fun line ->
    List.iter (words_of_line ~word_w:2 line) ~f:(fun word ->
      Format.printf "%a @." Bits.pp word;
      inputs.data := word;
      cycle ()));

  inputs.valid := Bits.gnd;

  (* wait for flush *)
  for _ = 1 to 3 do cycle () done;

  Format.printf "all bits[%d]=%a @." (Bits.width !bits) Bits.pp !bits;
  let _beams = Bits.split_msb ~exact:false ~part_width:6 !bits in
  (* List.iter (beams) ~f:(Format.printf "%a @." Bits.pp); *)

  (* List.iter (List.zip_exn (strip_and_filter_lines twolines) beams) ~f:(fun (line, beams) ->
    print_endline (line_of_bits ~line beams)); *)

  let result = Bits.to_int !(outputs.out) in
  Format.printf "result=%u@." result;
  Out_channel.flush oc;
  [%expect {|
    |}]

let%expect_test "Line input after clear, waveform capture" =
  let scope = Scope.create ~flatten_design:false () in
  let sim = Simulator.create ~config:(Cyclesim.Config.trace `All_named) (Laboratories.create scope) in
  let filename = "/tmp/laboratories.vcd" in
  let oc = Out_channel.create filename in
  let sim = Vcd.wrap oc sim in
  let inputs = Cyclesim.inputs sim in
  let cycle () = Cyclesim.cycle sim in
  inputs.valid := Bits.gnd;
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.vdd;
  inputs.data := bits_of_word 1;
  cycle ();
  inputs.data := bits_of_word 2;
  cycle ();
  inputs.valid := Bits.gnd;
  cycle ();
  Out_channel.close oc;
  [%expect {|
    |}]
