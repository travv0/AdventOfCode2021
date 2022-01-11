open Base

type 'a node = { f : int; elem : 'a } [@@deriving compare, sexp_of]

module type Elem = sig
  include Comparator.S

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val heuristic : t -> t -> int
  val equal : t -> t -> bool
  val neighbors : t -> (t * int) list
end

module Node (M : Elem) = struct
  module T = struct
    type t = M.t node [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let rec build_path came_from goal : 'a list =
  match Map.find came_from goal with
  | Some elem -> goal :: build_path came_from elem
  | None -> []

let path (type a) (elem : (module Elem with type t = a)) (start : a) (goal : a)
    : (a list * int) option =
  let module Elem = (val elem) in
  let module ElemNode = Node (Elem) in
  let h = Fn.flip Elem.heuristic goal in
  let open_set =
    ref @@ Set.singleton (module ElemNode) { elem = start; f = h start }
  in
  let came_from = ref @@ Map.empty (module Elem) in

  let g_score_map = ref @@ Map.singleton (module Elem) start 0 in
  let g_score node =
    Map.find !g_score_map node |> Option.value ~default:Int.max_value
  in

  let result = ref None in

  while Option.is_none !result && (not @@ Set.is_empty !open_set) do
    let current = Set.min_elt_exn !open_set in
    if Elem.equal current.elem goal then
      result := Some (List.rev (build_path !came_from goal), current.f)
    else (
      open_set := Set.remove_index !open_set 0;
      current.elem
      |> Elem.neighbors
      |> List.iter ~f:(fun (neighbor, d) ->
             let tentative_g_score = g_score current.elem + d in
             if tentative_g_score < g_score neighbor then (
               came_from :=
                 Map.update !came_from neighbor ~f:(Fn.const current.elem);
               g_score_map :=
                 Map.update !g_score_map neighbor
                   ~f:(Fn.const tentative_g_score);
               let f_score = tentative_g_score + h neighbor in
               if
                 not
                 @@ Set.exists !open_set ~f:(fun { elem; _ } ->
                        Elem.equal elem neighbor)
               then
                 open_set := Set.add !open_set { elem = neighbor; f = f_score })))
  done;

  !result
