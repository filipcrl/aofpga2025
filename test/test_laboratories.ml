open Core
open Hardcaml
open Adventoffpga
module Simulator = Cyclesim.With_interface (Laboratories.I) (Laboratories.O)

let example1 = [ 0; 1; 2; 3 ]

let bits_of_word word = Bits.of_int ~width:Laboratories.word_width word

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
