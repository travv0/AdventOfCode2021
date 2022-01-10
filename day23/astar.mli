module type Elem = sig
  type t
  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.t
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val heuristic : t -> t -> int
  val equal : t -> t -> bool
  val neighbors : t -> (t * int) list
end

val path : (module Elem with type t = 'a) -> 'a -> 'a -> ('a list * int) option
