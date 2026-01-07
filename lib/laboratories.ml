open Hardcaml
open Hardcaml.Signal

let w = 15

module I = struct
  type 'a t =
    { clock  : 'a
    ; clear  : 'a
    ; data   : 'a[@bits w]
    ; enable : 'a
    ; set    : 'a
    } [@@deriving hardcaml]
end

module O = struct
  type 'a t = 
    { out  : 'a[@bits 32]
    ; line : 'a[@bits w]
    } [@@deriving hardcaml]
end 

let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let line = reg_fb spec ~enable:i.enable ~width:w ~f:(fun line ->
    let linenosplit = (line &: ((~:) i.data)) in
    let splitr = (srl line 1) &: (srl i.data 1) in
    let splitl = (sll line 1) &: (sll i.data 1) in
    mux2 i.set i.data (splitr |: splitl |: linenosplit)) in
  let out = reg_fb spec ~enable:i.enable ~width:32 ~f:(fun out ->
    let pc32 = Signal.uresize (popcount (i.data &: line)) 32 in
    out +: pc32) in
  { O.out; line }