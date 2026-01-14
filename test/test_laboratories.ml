open Core
open Hardcaml
open Adventoffpga

let ceil_div a b = 1 + ((a - 1) / b) 

module Short_dut_1b = Laboratories.Make (struct
  let word_w = 1
  let line_w = 5
  let acc_w = 4
end)

module Short_dut_2b = Laboratories.Make (struct
  let word_w = 2
  let line_w = 5
  let acc_w = 4
end)

module Short_dut_3b = Laboratories.Make (struct
  let word_w = 3
  let line_w = 5
  let acc_w = 4
end)

module Big_dut_64b = Laboratories.Make (struct
  let word_w = 64
  let line_w = 141
  let acc_w = 32
end)

let short_input =
  {|
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

let padded_line_w ~line_w ~word_w = ceil_div line_w word_w * word_w

let words_of_line ~line_w ~word_w line =
  let open Bits in
  let line = String.strip line in
  if String.length line <> line_w
  then
    raise_s
      [%message
        "Line length doesn't match expected width"
          (line_w : int)
          (String.length line : int)
          (line : string)];
  let bits = bits_of_line line in
  let padded_w = padded_line_w ~line_w ~word_w in
  let extra = padded_w - width bits in
  Bits.split_msb ~part_width:word_w (sll (uresize bits padded_w) extra)

type gap =
  | Fixed of int
  | Random of
      { seed : int
      ; max_gap : int
      }

let next_gap_fn = function
  | Fixed n ->
    if n < 0 then invalid_arg "gap must be >= 0";
    fun () -> n
  | Random { seed; max_gap } ->
    if max_gap < 0 then invalid_arg "max_gap must be >= 0";
    let rng = Random.State.make [| seed |] in
    fun () -> Random.State.int rng (max_gap + 1)

let simulate_and_capture
  (module Dut : Laboratories_intf.S)
  ~word_w
  ~line_w
  ~lines
  ~gap
  =
  let words =
    List.concat_map lines ~f:(fun line -> words_of_line ~line_w ~word_w line)
  in
  let expected_words = List.length words in

  let scope = Scope.create ~flatten_design:false () in
  let module Simulator = Cyclesim.With_interface (Dut.I) (Dut.O) in
  let sim = Simulator.create (Dut.create scope) in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let outputs_after = Cyclesim.outputs sim in

  let captured_words_rev = ref [] in
  let captured_word_count = ref 0 in
  let capture_if_ready () =
    if !captured_word_count < expected_words
       && Bits.is_vdd !(outputs_before.beams_valid)
    then (
      captured_words_rev := !(outputs_before.beams_next) :: !captured_words_rev;
      incr captured_word_count)
  in
  let cycle () =
    Cyclesim.cycle sim;
    capture_if_ready ()
  in

  let next_gap = next_gap_fn gap in

  inputs.valid := Bits.gnd;
  inputs.clear := Bits.vdd;
  inputs.data := Bits.zero word_w;
  cycle ();
  inputs.clear := Bits.gnd;

  List.iter words ~f:(fun word ->
    inputs.valid := Bits.vdd;
    inputs.data := word;
    cycle ();
    inputs.valid := Bits.gnd;
    for _ = 1 to next_gap () do
      cycle ()
    done);

  inputs.valid := Bits.gnd;

  (* Flush until we have the full output, then wait for the design to go quiet. *)
  let quiet_cycles_required = 10 in
  let quiet_cycles = ref 0 in
  let max_cycles_after_input = expected_words * 100 + 1_000 in
  let cycles_after_input = ref 0 in
  while
    !cycles_after_input < max_cycles_after_input
    && (!captured_word_count < expected_words || !quiet_cycles < quiet_cycles_required)
  do
    incr cycles_after_input;
    cycle ();
    if Bits.is_vdd !(outputs_before.beams_valid)
    then quiet_cycles := 0
    else incr quiet_cycles
  done;

  if !captured_word_count <> expected_words
  then
    raise_s
      [%message
        "Timed out waiting for expected output words"
          (expected_words : int)
          (!captured_word_count : int)
          (max_cycles_after_input : int)];

  if !quiet_cycles < quiet_cycles_required
  then
    raise_s
      [%message
        "Timed out waiting for the design to go quiet"
          (quiet_cycles_required : int)
          (!quiet_cycles : int)
          (max_cycles_after_input : int)];

  let captured_words = List.rev !captured_words_rev in
  let beams_bits =
    match captured_words with
    | [] -> Bits.empty
    | _ -> Bits.concat_msb captured_words
  in
  let result = Bits.to_int !(outputs_after.out) in
  beams_bits, result

let run_short_input
  (module Dut : Laboratories_intf.S)
  ~word_w
  ~gap
  =
  let lines = strip_and_filter_lines short_input in
  let beams_bits, result =
    simulate_and_capture (module Dut) ~word_w ~line_w:5 ~lines ~gap
  in
  let beams =
    Bits.split_msb ~part_width:(padded_line_w ~line_w:5 ~word_w) beams_bits
  in
  List.iter (List.zip_exn lines beams) ~f:(fun (line, bits) ->
    print_endline (line_of_bits ~line bits));
  printf "result=%d\n" result

let run_bigtest
  (module Dut : Laboratories_intf.S)
  ~word_w
  ~gap
  =
  let sourceroot = Sys.getenv_exn "DUNE_SOURCEROOT" in
  let bigtest_path = Filename.concat sourceroot "test/bigtest.txt" in
  let lines = In_channel.read_all bigtest_path |> strip_and_filter_lines in
  let _beams_bits, result =
    simulate_and_capture (module Dut) ~word_w ~line_w:141 ~lines ~gap
  in
  printf "result=%d\n" result

let%expect_test "short input, 1b word, gap 0" =
  run_short_input (module Short_dut_1b) ~word_w:1 ~gap:(Fixed 0);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 1b word, gap 1" =
  run_short_input (module Short_dut_1b) ~word_w:1 ~gap:(Fixed 1);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 1b word, gap 2" =
  run_short_input (module Short_dut_1b) ~word_w:1 ~gap:(Fixed 2);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 1b word, seeded random gap" =
  run_short_input (module Short_dut_1b) ~word_w:1 ~gap:(Random { seed = 123; max_gap = 2 });
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 2b word, gap 0" =
  run_short_input (module Short_dut_2b) ~word_w:2 ~gap:(Fixed 0);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 2b word, gap 1" =
  run_short_input (module Short_dut_2b) ~word_w:2 ~gap:(Fixed 1);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 2b word, gap 2" =
  run_short_input (module Short_dut_2b) ~word_w:2 ~gap:(Fixed 2);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 2b word, seeded random gap" =
  run_short_input (module Short_dut_2b) ~word_w:2 ~gap:(Random { seed = 123; max_gap = 2 });
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 3b word, gap 0" =
  run_short_input (module Short_dut_3b) ~word_w:3 ~gap:(Fixed 0);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 3b word, gap 1" =
  run_short_input (module Short_dut_3b) ~word_w:3 ~gap:(Fixed 1);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 3b word, gap 2" =
  run_short_input (module Short_dut_3b) ~word_w:3 ~gap:(Fixed 2);
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "short input, 3b word, seeded random gap" =
  run_short_input (module Short_dut_3b) ~word_w:3 ~gap:(Random { seed = 123; max_gap = 2 });
  [%expect {|
    ..S..
    .|^|.
    |^||.
    |.||^
    result=2
    |}]

let%expect_test "bigtest, 64b word" =
  run_bigtest (module Big_dut_64b) ~word_w:64 ~gap:(Fixed 0);
  run_bigtest (module Big_dut_64b) ~word_w:64 ~gap:(Fixed 1);
  run_bigtest (module Big_dut_64b) ~word_w:64 ~gap:(Fixed 2);
  run_bigtest (module Big_dut_64b) ~word_w:64 ~gap:(Random { seed = 123; max_gap = 3 });
  [%expect {|
    result=1587
    result=1587
    result=1587
    result=1123
    |}]
