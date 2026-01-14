open Core
open Hardcaml
(* open Hardcaml_waveterm *)
open Adventoffpga

let ceil_div a b = 1 + ((a - 1) / b) 

module Dut1x5 = Laboratories.Make (struct
  let word_w = 1
  let line_w = 5
  let acc_w = 4
end)

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
    match Bits.bit bits (Bits.width bits-i-1) |> Bits.is_vdd with
    | true when Char.(c <> 'S') -> '|'
    | _ -> c)

let words_of_line ~word_w line =
  let open Bits in
  let bits = bits_of_line line in
  let num_of_bits = ceil_div (width bits) word_w * word_w in
  let extra = num_of_bits - (width bits) in
  Bits.split_msb ~part_width:word_w (sll (uresize bits num_of_bits) extra)

let%expect_test "short input, 2 clock gap" =
  let scope = Scope.create ~flatten_design:false () in
  let module Simulator = Cyclesim.With_interface (Dut1x5.I) (Dut1x5.O) in
  let sim = Simulator.create ~config:Cyclesim.Config.trace_all (Dut1x5.create scope) in
  let filename = "/tmp/short_input.vcd" in
  let oc = Out_channel.create filename in
  let sim = Vcd.wrap oc sim in
  let inputs = Cyclesim.inputs sim in
  (* Need to sample before the clock edge since beams_valid is combinational *)
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  let bits = ref Bits.empty in
  let cycle_ = ref 0 in
  let cycle () =
    cycle_ := !cycle_ + 1;
    Cyclesim.cycle sim;
    (* Format.printf "beams_valid [%d] %a @." !cycle_ Bits.pp !(outputs.beams_valid); *)
    if Bits.is_vdd !(outputs.beams_valid) then
     ((*Format.printf "received [%d] %a @." !cycle_ Bits.pp !(outputs.beams_next);*)
      bits := Bits.concat_msb_e [(!bits); !(outputs.beams_next)]);
  in
  inputs.valid := Bits.gnd;
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.vdd;

  List.iter (strip_and_filter_lines twolines) ~f:(fun line ->
    List.iter (words_of_line ~word_w:1 line) ~f:(fun word ->
      (* Format.printf "%a @." Bits.pp word; *)
      inputs.valid := Bits.vdd; 
      inputs.data := word;
      cycle ();
      inputs.valid := Bits.gnd; 
      cycle ();
      cycle ();
      cycle ()));

  inputs.valid := Bits.gnd;

  (* wait for flush *)
  for _ = 1 to 3 do cycle () done;

  Format.printf "all bits[%d]=%a @." (Bits.width !bits) Bits.pp !bits;
  let beams = Bits.split_msb ~exact:false ~part_width:5 !bits in
  List.iter (beams) ~f:(Format.printf "%a @." Bits.pp);

  List.iter (List.zip_exn (strip_and_filter_lines twolines) beams) ~f:(fun (line, beams) ->
    (*Format.printf "line=%s beam=%a @." line Bits.pp beams;*)
    print_endline (line_of_bits ~line beams));

  let result = Bits.to_int !(outputs.out) in
  Format.printf "result=%u@." result;
  Out_channel.flush oc;
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]
  
let capture_lines sim out beams_valid beams_next ~f =
  let bits = ref Bits.empty in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !beams_valid then
      bits := Bits.concat_msb_e [!bits; !beams_next] in

  f cycle;

  let beams = Bits.split_msb ~exact:false ~part_width:5 !bits in
  List.iter (List.zip_exn (strip_and_filter_lines twolines) beams) ~f:(fun (line, beams) ->
    print_endline (line_of_bits ~line beams));

  let result = Bits.to_int !out in
  Format.printf "result=%u@." result

let%expect_test "short input, 1 cycle gap" =
  let scope = Scope.create ~flatten_design:false () in
  let module Simulator = Cyclesim.With_interface (Dut1x5.I) (Dut1x5.O) in
  let sim = Simulator.create ~config:Cyclesim.Config.trace_all (Dut1x5.create scope) in
  let inputs = Cyclesim.inputs sim in
  (* Need to sample before the clock edge since beams_valid is combinational *)
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  capture_lines sim outputs.out outputs.beams_valid outputs.beams_next
    ~f:(fun cycle ->
      inputs.valid := Bits.gnd;
      inputs.clear := Bits.vdd;
      cycle ();
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;

      List.iter (strip_and_filter_lines twolines) ~f:(fun line ->
        List.iter (words_of_line ~word_w:1 line) ~f:(fun word ->
          (* Format.printf "%a @." Bits.pp word; *)
          inputs.valid := Bits.vdd;
          inputs.data := word;
          cycle ();
          inputs.valid := Bits.gnd;
          cycle ()));

      inputs.valid := Bits.gnd;

      (* wait for flush *)
      for _ = 1 to 3 do cycle () done);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, no gaps" =
  let scope = Scope.create ~flatten_design:false () in
  let module Simulator = Cyclesim.With_interface (Dut1x5.I) (Dut1x5.O) in
  let sim = Simulator.create ~config:Cyclesim.Config.trace_all (Dut1x5.create scope) in
  let inputs = Cyclesim.inputs sim in
  (* Need to sample before the clock edge since beams_valid is combinational *)
  let outputs = Cyclesim.outputs ~clock_edge:Before sim in

  capture_lines sim outputs.out outputs.beams_valid outputs.beams_next
    ~f:(fun cycle ->
      inputs.valid := Bits.gnd;
      inputs.clear := Bits.vdd;
      cycle ();
      inputs.clear := Bits.gnd;
      inputs.valid := Bits.vdd;

      List.iter (strip_and_filter_lines twolines) ~f:(fun line ->
        List.iter (words_of_line ~word_w:1 line) ~f:(fun word ->
          (* Format.printf "%a @." Bits.pp word; *)
          inputs.data := word;
          cycle ()));

      inputs.valid := Bits.gnd;

      (* wait for flush *)
      for _ = 1 to 3 do cycle () done);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

(* let%expect_test "Line input after clear, waveform capture" =
  let scope = Scope.create ~flatten_design:false () in
  let sim = Simulator.create ~config:(Cyclesim.Config.trace `All_named) (Laboratories.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let n = ref 0 in
  let cycle () =
    Cyclesim.cycle_before_clock_edge sim;
    Format.printf
      "%d v=%a d=%a bv=%a bn=%a @."
      !n
      Bits.pp
      !(inputs.valid)
      Bits.pp
      !(inputs.data)
      Bits.pp
      !(outputs_before.beams_valid)
      Bits.pp
      !(outputs_before.beams_next);
    Cyclesim.cycle_at_clock_edge sim;
    n := !n + 1;
    Cyclesim.cycle_after_clock_edge sim;
  in
  inputs.valid := Bits.gnd;
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.valid := Bits.vdd;
  let bits = words_of_line ~word_w:2 "..S.." in
  List.iter bits ~f:(fun word ->
    inputs.data := word;
    cycle ());
  inputs.valid := Bits.gnd;
  cycle ();
  Waveform.print waves;
  [%expect {|
    0 v=0 d=00 bv=0 bn=00
    line=00100
    1 v=1 d=00 bv=1 bn=00
    2 v=1 d=10 bv=1 bn=10
    3 v=1 d=00 bv=1 bn=00
    4 v=0 d=00 bv=0 bn=00
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clear          ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │data           ││ 0              │2      │0                         │
    │               ││────────────────┴───────┴───────────────           │
    │valid          ││        ┌───────────────────────┐                  │
    │               ││────────┘                       └───────           │
    │               ││────────────────┬───────┬───────────────           │
    │beams_next     ││ 0              │2      │0                         │
    │               ││────────────────┴───────┴───────────────           │
    │beams_valid    ││        ┌───────────────────────┐                  │
    │               ││────────┘                       └───────           │
    │               ││────────────────────────────────────────           │
    │out            ││ 00                                                │
    │               ││────────────────────────────────────────           │
    │               ││────────────────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
*)