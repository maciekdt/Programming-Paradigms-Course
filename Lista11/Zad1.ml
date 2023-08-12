type comparisonResult = Less  | Equal | Greater ;;


module type COMPARATOR =
  sig
    type t
    val compare: t * t -> comparisonResult
  end;;


module IntComparator: COMPARATOR with type t = int =
  struct
      type t = int
      let compare(i1, i2) = 
          if i1 = i2 then Equal
          else if i1 > i2 then Greater
          else Less
  end;;

  module IntPairComparator: COMPARATOR with type t = int * int =
  struct
      type t = int * int
      let compare(s1, s2) = 
          if s1 = s2 then Equal
          else match (s1, s2) with
            |((s11, s12), (s21, s22)) ->(
              if(s11 + s12 > s21 + s22) then Greater
              else Less
            )
  end;;


module type QUEUE =
  functor (Comparator : COMPARATOR) ->
    sig
      type 'a queue = EmptyQueue | Coponenet of Comparator.t * Comparator.t queue
      exception Empty of string

      val empty : 'a queue
      val enqueue : Comparator.t queue * Comparator.t -> Comparator.t queue
      val dequeue : 'a queue -> Comparator.t queue
      val first : 'a queue -> Comparator.t
      val toList : Comparator.t queue -> Comparator.t list
      val count : Comparator.t queue * Comparator.t *  comparisonResult -> int
    end;;


module Queue:QUEUE =
  functor (Comparator: COMPARATOR) ->
    struct
      type 'a queue = EmptyQueue | Coponenet of Comparator.t * Comparator.t queue
      exception Empty of string

      let empty = EmptyQueue

      let rec enqueue(q, element) = 
        match q with
          | EmptyQueue -> Coponenet(element, EmptyQueue)
          | Coponenet(head, tail) -> Coponenet(head, enqueue(tail, element))

      let dequeue q = 
        match q with
          | EmptyQueue -> EmptyQueue
          | Coponenet(head, tail) -> tail
      
      let first q = 
        match q with
          | EmptyQueue -> raise (Empty "module Queue: first()")
          | Coponenet(head, tail) -> head
      
      let toList q = 
        let rec toListInner(q, list) =
          match q with
            | EmptyQueue -> list
            | Coponenet(head, tail) -> toListInner(tail, head::list) 
          in
        List.rev(toListInner(q, []))

      let count(q, value, comparison) =
          let rec countInner(q, counter) =
            match q with
              | EmptyQueue -> counter
              | Coponenet(head, tail) -> (
                if(Comparator.compare(head, value) = comparison) then countInner(tail, counter + 1)
                else countInner(tail, counter)
              )
          in
          countInner(q, 0)

    end;; 

module IntQueue = Queue(IntComparator);;
module IntPairQueue = Queue(IntPairComparator);;

let q = IntQueue.empty;;

let q = IntQueue.enqueue(q, 1);;
let q = IntQueue.enqueue(q, 2);;
let q = IntQueue.enqueue(q, 3);;
let q = IntQueue.enqueue(q, 4);;



(*open Printf
let a = IntQueue.toList(q);;
let () = List.iter (printf "%d ") a;;

print_int(IntQueue.count(q, 2, Greater));;*)


let q2 = IntPairQueue.empty;;
let q2 = IntPairQueue.enqueue(q2, (2,3));;
let q2 = IntPairQueue.enqueue(q2, (1,1));;
let q2 = IntPairQueue.enqueue(q2, (3,3));;



print_int(IntPairQueue.count(q2, (1,1), Greater));;


