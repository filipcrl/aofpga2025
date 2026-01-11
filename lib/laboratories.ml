open Hardcaml
open Hardcaml.Signal

let word_width = 4
let line_width = 15

let acc_width = 8

let counter_max =
  Float.of_int line_width /. Float.of_int word_width
  |> Float.ceil |> Float.to_int

let counter_w =
  Float.of_int counter_max |> Float.log2 |> Float.ceil |> Float.to_int

module I = struct
  type 'a t =
    { clock  : 'a
    ; clear  : 'a
    ; data   : 'a[@bits word_width]
    ; enable : 'a
    ; set    : 'a
    } [@@deriving hardcaml]
end

module O = struct
  type 'a t = 
    { out : 'a[@bits acc_width]
    } [@@deriving hardcaml]
end

module State = struct
  type t =
    | Start
    | Process
    | Last
    [@@deriving sexp_of, compare ~localize, enumerate]
end

let line_next_ set last line line_next cur next prev_bit =
  let w = width line in
  let hits = line &: cur in
  let hits_next = bit line_next 0 &: bit next 0 in
  let linenosplit = line &: (~: cur) in
  let carry_in_l = uresize prev_bit w in
  let next_lsb = mux2 last gnd hits_next in
  let carry_in_r = sll (uresize next_lsb w) (w - 1) in
  let splitl = (sll hits 1) |: carry_in_l in
  let splitr = (srl hits 1) |: carry_in_r in
  let computed_next = linenosplit |: splitl |: splitr in
  let prev_bit_next = bit hits (w - 1) in
  mux2 set cur computed_next, prev_bit_next

let create _scope ({ clock; clear; data; enable; set } : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in

  let acc = Variable.reg ~enable ~width:acc_width spec in
  let buf = Variable.reg ~enable ~width:word_width spec in
  let counter = Variable.reg ~enable ~width:counter_w spec in
  let prev_bit = Variable.reg ~enable ~width:counter_w spec in
  let sm = State_machine.create ~enable (module State) spec in

  let last = counter.value ==:. counter_max - 1 in
  let line_next, prev_bit_next = line_next_ set last line line_next buf.value data prev_bit in

  [ sm.switch
    [ (Start,
      [ counter <--. 0
      ; prev_bit <--. 0
      ; sm.set_next Process
      ])
    ; (Process,
      [ counter <-- counter.value +:. 1
      ; line <-- line_next
      ; prev_bit <-- prev_bit_next
      ; when_ (last) [sm.set_next Start]
      ])
    ]
  ; buf <-- data
  ] |> compile;
  { O.out=acc }

(*
waiting for data
process
last
*)

(* let create (i : _ I.t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let line = reg_fb spec ~enable:i.enable ~width:w ~f:(fun line ->
    let linenosplit = (line &: ((~:) i.data)) in
    let splitr = (srl line 1) &: (srl i.data 1) in
    let splitl = (sll line 1) &: (sll i.data 1) in
    mux2 i.set i.data (splitr |: splitl |: linenosplit)) in
  let out = reg_fb spec ~enable:i.enable ~width:acc_width ~f:(fun out ->
    let pc32 = Signal.uresize (popcount (i.data &: line)) 32 in
    out +: pc32) in
  { O.out; line } *)