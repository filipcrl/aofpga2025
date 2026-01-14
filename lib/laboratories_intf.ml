module type Config = sig
  val word_w : int
  val line_w : int
  val acc_w : int
end

module type S = sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; valid : 'a
      ; data  : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { out         : 'a
      ; beams_next  : 'a
      ; beams_valid : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Hardcaml.Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
end

module type Laboratories = sig
  module Make (_: Config) : S
end
