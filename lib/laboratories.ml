open Core
open Hardcaml
open Hardcaml.Signal

let ceil_div a b = 1 + (a - 1) / b 

let word_width = 4
let line_width = 15

let acc_width = 8

let counter_max = ceil_div line_width word_width
let counter_w = Int.ceil_log2 counter_max

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid : 'a
    ; data  : 'a[@bits word_width]
    ; set   : 'a
    } [@@deriving hardcaml]
end

module O = struct
  type 'a t = 
    { out       : 'a[@bits acc_width]
    ; line_next : 'a[@bits word_width]
    } [@@deriving hardcaml]
end

module State = struct
  type t =
    | Start
    | Process
    | Last
    [@@deriving sexp_of, compare ~localize, enumerate]
end

let line_next_ set is_last line line_next cur next prev_bit =
  let w = width line in
  let hits = line &: cur in
  let hits_next = line_next &: next in
  let linenosplit = line &: (~: cur) in
  let carry_in_right = sll (uresize prev_bit w) (w - 1) in
  let split_right = (srl hits 1) |: carry_in_right in
  let next_msb = mux2 is_last gnd (bit hits_next (w - 1)) in
  let carry_in_left = uresize next_msb w in
  let split_left = (sll hits 1) |: carry_in_left in
  let computed_next = linenosplit |: split_left |: split_right in
  mux2 set cur computed_next, bit hits 0, popcount hits
  
let dual_port_ram ~clock ~write_address ~write_enable ~write_data ~read_address =
  let spec = Reg_spec.create ~clock () in
  (multiport_memory
     Int.(2 ** (width write_address))
     ~write_ports:
       [| { write_clock = clock
          ; write_enable
          ; write_address
          ; write_data
          }
       |]
     ~read_addresses:[| read_address |]).(0)
  |> reg spec

let create _scope ({ clock; clear; data; enable; set } : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in

  let acc = Variable.reg ~width:acc_width spec in
  let buf = Variable.reg ~width:word_width spec in
  let counter = Variable.reg ~width:counter_w spec in
  let prev_bit = Variable.reg ~width:counter_w spec in
  let buf_sel = Variable.reg ~width:1 spec in 

  let a = wire 1 in

  let last = counter.value ==:. counter_max - 1 in
  let line_next, prev_bit_next, count =
    line_next_ set last line line_next
      buf.value data prev_bit.value in

  let sm = State_machine.create (module State) spec in

  let write_enable = sm.is Process in
  let write_address = concat_lsb [~: (buf_sel.value); counter.value] in
  let read_address = concat_lsb [buf_sel.value; counter.value] in

  let line = dual_port_ram ~clock ~write_address ~write_enable ~write_data:line_next ~read_address in

  [ sm.switch
    [ (Start,
      [ counter <--. 0
      ; prev_bit <--. 0
      ; buf_sel <--. 0
      ; sm.set_next Process
      ])
    ; (Process,
      [ counter <-- counter.value +:. 1
      ; prev_bit <-- prev_bit_next
      ; acc <-- acc.value +: count
      ; when_ (last) [sm.set_next Start]
      ])
    ]
  ; buf <-- data
  ] |> compile;
  { O.out=acc.value; line_next }