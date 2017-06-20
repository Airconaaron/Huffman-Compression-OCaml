type 'a heap = {
  comp : 'a -> 'a -> bool;
  arr : 'a array;
  size : int ref
}

exception Empty
exception Full

(* size -> comparison function -> input array -> heap, raise Full when size is too big *)
val heap_of_array : int -> ('a -> 'a -> bool) -> 'a array -> 'a heap

val remove_min : 'a heap -> 'a

val insert : 'a heap -> 'a -> unit
