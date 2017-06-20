open Io;;
open Heap;;
open Printf;; 

(*First read in the tree so It can be used*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let valtree tree = 
match tree with
Br (a, _, _) -> a
| Lf -> []
;;

exception EmptyStack;;
exception InvalidFile;;
let push x s = x :: s;;
let pop s =
match s with
| x :: s' -> x
| [] -> raise EmptyStack;;
let tail s =
match s with
|x::s' -> s'
| [] -> raise EmptyStack;
;;

(*tail2 is used to mutate the stack such that it takes out the two elements we've been playign with *)
let tail2 s =
match s with
|x::a::s' -> s'
| _ -> raise EmptyStack
;;


let implode2 l =
let res = Bytes.create (List.length l) in
let rec imp i = function
| [] -> res
| c :: l -> Bytes.set res i  c; imp (i + 1) l in
imp 0 l;;



(*Turns our string into a char list. Fromm the ocaml guide*)
let explode s =
let rec exp i l =
if i < 0 then l else exp (i - 1) (s.[i] :: l) in
exp (String.length s - 1) [];;

(*Familiar functsion to homework 5a from last year*)
let rec loadtreehelper ls tree_stack =
match ls with 
|[] -> pop tree_stack
|h::t -> match h with 
| "/" -> let l = pop tree_stack in (*a random node*)
let r = pop (tail tree_stack) in
let operation = Br ('\\', l, r) in 
loadtreehelper t (push operation (tail2 tree_stack)) 
| a -> let newbr = Br ((char_of_int (int_of_string a)), Lf, Lf) in loadtreehelper t (push newbr tree_stack) (*important end nodes*)
;;

(*makes a list from the tree we put in*)
let rec tree_conv ls store acc=
match ls with
|[] -> acc 
|'/'::t -> tree_conv t store ("/"::acc)
|'|'::t -> tree_conv t [] ((implode2 (List.rev store))::acc)
|a::t ->  tree_conv t (a::store) acc

;;

(*reads in a line to create a char list*)
let tree_lister ic = 
let s = input_line ic in
explode s
;;

let load_tree ic =
let ls = tree_lister ic in
let conv = tree_conv ls [] [] in 
try loadtreehelper (conv) [] with
EmptyStack -> raise InvalidFile (* If i can't build a tree then the file must be messed up*)

;;

(*This thing will keep grabbing us a bit nice until EOF*)
(*each bit it grabs will instruct it on how to traverse our treee*)
let rec grabchar input_bits tree =
match tree with 
|Br (k, Lf, Lf) -> k (*when it reaches an important node it returns the char stored within*)
|Br (k, l, r) -> let nextio = getbit input_bits  in 
if not(nextio) (*true = right*) then grabchar input_bits l else
grabchar input_bits r
|_ -> '\n'
;;

(*writes the char we grabbed into the output file*)
let typewriter ic oc tree =
let newin = input_of_channel ic in 
let newin1 = input_bits_of_input newin in 
try 
while true do 
let char1 = grabchar newin1 tree in
output_char oc char1
done
with 
End_of_file -> ()
;;

let decompress infile outfile = 
(*Do a check for if the file is empty*)
let ic = open_in infile in
let oc = open_out outfile in
let decodetree = load_tree ic in
typewriter ic oc decodetree;
close_in ic; 
close_out oc; 
;;


decompress (Sys.argv).(1) (Sys.argv).(2);;

(*Compilation code
ocamlc heap.mli heap.ml io.mli io.ml  decompress.ml -o notweeester
*)
(*execute with ./notweeester "input file name" "output filename"*)