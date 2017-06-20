open Io;;
open Heap;;
open Printf;; 

(*Tree functions*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let valtree tree = 
match tree with
Br (a, _, _) -> a
| Lf -> []
;;

(*Helps when we want to save our tree*)
exception EmptyStack;;
let push x s = x :: s;;
let pop s =
  match s with
    | x :: s' -> x
    | [] -> raise EmptyStack;;
let tail s =
  match s with
  |x::s' -> s'
  | [] -> raise EmptyStack;;
;;

(*tail2 is used to mutate the stack such that it takes out the two elements we've been playign with *)
let tail2 s =
  match s with
  |x::a::s' -> s'
  | _ -> raise EmptyStack;;
;;

(*let this be the function that builds the huffman encoding *)
let compress_helper1 in_channel arr =
  let rec compress_helper1_inner in_channel1 =
    (*read everythign character by character*)
    let newchar = input_char in_channel1 in
    let pos = int_of_char newchar in
      arr.(pos) <- arr.(pos) + 1; (*places everything in a 0-255 array, using int_of_char as indices*)
      compress_helper1_inner in_channel1
  in
    try compress_helper1_inner in_channel with (*You're gonna need to reach end of file eventually*)
        End_of_file -> ();;

(*This one builds our coding tree from an array *)
(*helper function*)
let copier oldarr newarr =
let placer = ref 0 in
for x = 0 to 255 do 
if oldarr.(x) > 0 then (*Only does something if the current position has a char*)
begin
let newchar = char_of_int x in
newarr.(!placer) <- (Br([newchar], Lf, Lf), oldarr.(x));
placer := !placer + 1
end
done

;;

(*Our comparison function for use with heap.ml*)
let comp x y =
let (_, a) = x in
let (_, b) = y in
if a < b then true else false
;;

let hufflepuff arr =
let count = Array.fold_left (fun x y -> if y > 0 then (x + 1) else x) 0 arr in
let newarr = Array.make count (Lf, 0) in 
copier arr newarr;
heap_of_array count comp newarr
;;
(*Our heap is a tuple (tree structure, char itself)*)

let remove_2min_help heap =
let (char1, a) = remove_min heap in
let (char2, b) = remove_min heap in (*remove two from the heap*)
let val1 = valtree char1 in
let val2 = valtree char2 in
insert heap (Br(val1 @ val2, char1, char2), a+b);; (*recombine and add back inside the heap*)

let remove_2_plz heap =
try remove_2min_help heap with 
Empty -> () (*wrapper function*)
;;

let rec remove_2 heap count = (*recursive calls*)
if count == 0 then () else
begin 
remove_2_plz heap;
remove_2 heap (count -1) (*count is passed in from the number of non-empty cells in the array*)
end 
;;

(**This thing traverses our tree node by node, keeping a 0 if it goes left and 1 if it ravels right. Thus it adds all these encodings with the char into a list *)
let rec encodinghelper tree encoding acc=
match tree with
|Br ([a], Lf, Lf ) -> acc := (a, (List.rev encoding))::!acc
|Br (_, l, r) -> encodinghelper l (0:: encoding) acc; encodinghelper r (1::encoding) acc
|_ -> ()

;;
(*This translates the list into a more useful array. This array allows us to you int_of_char to easily find encodings*)
let rec placer encoding newarr =
match encoding with
|(a,b)::t -> let wow = int_of_char a in newarr.(wow) <- b; placer t newarr
|[] -> ()
;;

(*turns our list of ints into a string*)
let implode l =
let l = List.map (fun x -> x + 48) l in (*This makes it the value of the ascii character 0 or 1*)
let res = Bytes.create (List.length l) in
let rec imp i = function
| [] -> res
| c :: l -> Bytes.set res i  (char_of_int c); imp (i + 1) l in
imp 0 l;;
(*Now implode is kinda obsolete now*)

(*Original implementation* found on ocaml.org. cponverts list of chars into a string*)
let implode2 l =
let res = Bytes.create (List.length l) in
let rec imp i = function
| [] -> res
| c :: l -> Bytes.set res i  c; imp (i + 1) l in
imp 0 l;;


let rec converter encoder newlist =
match encoder with
|(a,b)::t -> converter t ((newlist := (a, implode b)::!newlist);newlist)
|[] -> ()
;;

(*NICE IT WORKS *)
let encodingbiatch tree newarr =
let sweet = ref [] in 
encodinghelper tree [] sweet;  (*sweet is a list of tuples of characters and encodings*)
(*Now we give a new list to placer which is of char, int list*)
placer !sweet newarr
;;


(*let this be the shit that prints out the bits I guess*)

let rec save_tree_helper (tr: 'char tree)  ch =
match tr with
|Br ([k], Lf, Lf) -> output_string ch (string_of_int(int_of_char k)); output_char ch '|'
|Br (k, l, r)-> 
output_string ch "/";
save_tree_helper l ch;
save_tree_helper r ch;
|Lf ->  output_string ch "L"
;;
(* write down braches as /, each important node write down the int_if_char followed by |*)


let save_tree oc (tr: 'a tree) =
save_tree_helper tr oc;
(*What should be our terminator. \n will do so we can use readline later*)
output_char oc '\n';
;;

(*Pass a list of 0s1s and it writes them in as bits*)
let rec typewritter output_bits ls =
match ls with 
[] -> ()
|h::t -> if h == 0 then 
begin putbit output_bits false;typewritter output_bits t end 
else (putbit output_bits true;typewritter output_bits t)

;;
(*in putbit true for 1 flase for zero*)

let writerhelp ic oc arr =
let newout = output_of_channel oc in
let newout1 = output_bits_of_output newout in 
let rec write1_inner ic1 =
(*read everythign character by character*)
let newchar = input_char ic1 in
let pos = int_of_char newchar in (*find position in the encoding arrau*)
let code = arr.(pos) in
typewritter newout1 code; (* write bits and repeat*)
write1_inner ic1
in
try write1_inner ic with (*You're gonna need to reach end of file eventually*)
End_of_file -> flush newout1;;
(*you'll need to call flush here remember*)
;;


let writingshit ic oc arr tree =
(*First write the tree inside I guess*)
save_tree oc tree;
(*then by referring to the encoded array we use io.ml to do the dirty writing work*)
writerhelp ic oc arr
;;

let compress infile outfile = 
let ch1 = open_in infile in
let newarr = Array.make 256 0 in
compress_helper1 ch1 newarr;(*collect the chars frequencies*)
close_in ch1;
(*Coder gives us the heap we will be using*)
let coder = hufflepuff newarr in
let sizer = coder.size in
remove_2 coder (!sizer);
(*creates our tree by removing min twice, combining and inserting back*)
let (weirdasstree,checker) = coder.arr.(0) in
(*So lets put this encoding back into another array. Allows us to the int_of_char*)
let encodedarr = Array.make 256 [] in
encodingbiatch weirdasstree encodedarr; (* puts the encoding within that array*)
let ch2 = open_out outfile in
let ch3 = open_in infile in
(*function that does actually writing*)
writingshit ch3 ch2 encodedarr weirdasstree;
close_out ch2;
close_in ch3
;;
compress (Sys.argv).(1) (Sys.argv).(2);;

(*Compilation code
ocamlc heap.mli heap.ml io.mli io.ml  compress.ml -o tweeester
*)
(*execute with ./tweeester "input file name" "output filename"*)

(*Truly the output is the stuff of nightmares though*)
