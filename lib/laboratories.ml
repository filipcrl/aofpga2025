open Core
open Hardcaml
open Hardcaml.Signal

let ceil_div a b = 1 + (a - 1) / b 

let word_width = 2
let line_width = 3

let acc_width = 8

let counter_max = ceil_div line_width word_width
let counter_w = Int.ceil_log2 counter_max

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid : 'a
    ; data  : 'a[@bits word_width]
    } [@@deriving hardcaml]
end

module O = struct
  type 'a t = 
    { out        : 'a[@bits acc_width]
    ; beams_next : 'a[@bits word_width]
    } [@@deriving hardcaml]
end

module State = struct
  type t = Set | Wait | Run | Flush
    [@@deriving sexp_of, compare ~localize, enumerate]
end

(*
  beams1     beams0 
  splitters1 splitters0
  ---------------------
  beams_next
*)
let propagate_beams ~last ~beams:(beams1, beams0) ~splitters:(splitters1, splitters0) ~prev_bit =
  let w = width beams1 in
  let hits = beams1 &: splitters1 in
  let hits_next = beams0 &: splitters0 in
  let linenosplit = beams1 &: (~: splitters1) in
  let carry_in_right = sll (uresize prev_bit w) (w - 1) in
  let split_right = (srl hits 1) |: carry_in_right in
  let next_msb = mux2 last gnd (bit hits_next (w - 1)) in
  let carry_in_left = uresize next_msb w in
  let split_left = (sll hits 1) |: carry_in_left in
  let beams_next = linenosplit |: split_left |: split_right in
  beams_next, bit hits 0, popcount hits

(** Will store the ram and *)
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

module Shift_register = struct
  type t = Always.Variable.t array

  let create ~width ~n spec =
    Array.init n ~f:(fun _ -> Always.Variable.reg ~width spec)

  let shift t data =
    let open Always in
    Array.mapi t ~f:(fun i reg ->
      match i with
      | 0 -> reg <-- data
      | i -> reg <-- t.(i-1).value)
    |> Array.to_list |> proc
end

let create scope ({ clock; clear; valid; data } : _ I.t) =
  let open Always in
  let spec = Reg_spec.create ~clock ~clear () in

  let%hw_var acc = Variable.reg ~width:acc_width spec in
  let counter = Variable.reg ~width:counter_w spec in
  let counter_sr = Shift_register.create ~width:counter_w ~n:2 spec in

  let%hw_var flush_cnt = Variable.reg ~width:2 spec in

  let splitters = Shift_register.create ~width:word_width ~n:2 spec in
  let beams1 = Variable.reg ~width:word_width spec in

  let prev_bit = Variable.reg ~width:1 spec in
  let buf_sel = Variable.reg ~width:1 spec in 

  let sm = State_machine.create (module State) spec in

  let beams0 = wire word_width in
  let last = counter.value ==:. counter_max - 1 in
  let beams_next, prev_bit_next, count = propagate_beams
    ~last ~beams:(beams1.value, beams0)
    ~splitters:(splitters.(1).value, splitters.(0).value) ~prev_bit:prev_bit.value in

  let%hw write_enable = ~:(sm.is Wait) in
  let%hw write_address = concat_msb [buf_sel.value; mux2 (sm.is Set) counter.value counter_sr.(1).value] in
  let%hw write_data = mux2 (sm.is Set) data beams_next in
  let%hw read_address = concat_msb [buf_sel.value; counter.value] in

  beams0 <== dual_port_ram ~clock ~write_address ~write_enable ~write_data ~read_address;

  (* Advance the pipeline *)
  let advance () =
    [ splitters.(1) <-- splitters.(0).value
    ; splitters.(0) <-- data
    ; beams1 <-- beams0
    ] |> proc in

  [ sm.switch
    [ (Set,
      [ when_ valid [counter <-- counter.value +:. 1]
      ; when_ last [sm.set_next Wait]
      ])
    ; (Wait,
      [ when_ valid
        [ advance ()
        ; Shift_register.shift counter_sr counter.value
        ; counter <-- counter.value +:. 1
        ; when_ (counter.value ==:. 1) [sm.set_next Run]
        ]])
    ; (Run,
      [ when_ valid
        [ counter <-- counter.value +:. 1
        ; prev_bit <-- prev_bit_next
        ; acc <-- acc.value +: uresize count acc_width
        ; advance ()
        ; when_ last
          [ counter <--. 0
          ; buf_sel <-- ~:(buf_sel.value)
          ; flush_cnt <--. 2
          ; sm.set_next Flush
          ]
        ]])
    ; (Flush,
      [ flush_cnt <-- flush_cnt.value -:. 1
      ; advance ()
      ; acc <-- acc.value +: uresize count acc_width
      ; prev_bit <-- prev_bit_next
      ; when_ valid [ counter <-- counter.value +:. 1 ]
      ; when_ (flush_cnt.value ==:. 0)
        [ if_ (valid &&: counter.value ==:. 1)
          [sm.set_next Run]
          [sm.set_next Wait]
        ]
      ])
    ]
  ] |> compile;
  { O.out=acc.value; beams_next }