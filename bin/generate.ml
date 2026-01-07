open! Core
open! Hardcaml
open! Adventoffpga

let () =
  let module C = Circuit.With_interface (Laboratories.I) (Laboratories.O) in
  let circuit = C.create_exn ~name:"laboratory_top" Laboratories.create in
  Rtl.print Verilog circuit
