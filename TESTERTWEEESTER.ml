
open Printf;; 
type 'a heap = {
  comp : 'a -> 'a -> bool;
  arr : 'a array;
  size : int ref
}
(*Tree functions*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let valtree tree = 
  match tree with
      Br (a, _, _) -> a
    | Lf -> []
;;

exception EmptyStack;;
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

(* Reverse function *)
let rec rev_inner a l =
  match l with
    |[] -> a
    |h::t -> rev_inner (h::a) t
;;

let rev l =
  rev_inner [] l;;
let c = 0;;

char_of_int c == '0';;

exception Empty
exception Full

type 'a heap = {
  comp : 'a -> 'a -> bool;
  arr : 'a array;
  size : int ref
};;

let swap arr i j =
  let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t;;

let isroot n = n = 0;;
let left n = ((n + 1) * 2) - 1;;
let right n = (n + 1) * 2;;
let parent n = ((n + 1) / 2) - 1;;

let rec swim comp arr i =
  if isroot i then () else
  if comp arr.(i) arr.(parent i) then 
    begin
      swap arr i (parent i);
      swim comp arr (parent i)
    end

let rec sink comp size arr i = 
  if left i >= size then () else
  if right i >= size then
    begin
      if comp arr.(left i) arr.(i) then swap arr (left i) i
    end else 
    begin
      if comp arr.(left i) arr.(i) ||
         comp arr.(right i) arr.(i) then
        begin
          if comp arr.(left i) arr.(right i) then 
            begin
              swap arr i (left i);
              sink comp size arr (left i)
            end else 
            begin
              swap arr i (right i);
              sink comp size arr (right i)
            end
        end
    end

exception Full;;
let insert {comp; arr; size} item =
  let sz = ! size in
    if sz = Array.length arr then raise Full else
      begin
        arr.(sz) <- item;
        swim comp arr sz;
        size := sz + 1
      end;;

let printtruth io =
  if io then 
    begin
      print_int 1;
      print_newline ()
    end
  else 
    begin
      print_int 0;
      print_newline ()
    end
;;


exception Empty;;
let remove_min {comp; arr; size} =
  let sz = !size - 1 in
    if sz = -1 then raise Empty else
      begin
        size := sz;
        swap arr 0 sz;
        sink comp sz arr 0;
        arr.(sz)
      end;;

let heap_of_array size comp arr =
  if size > Array.length arr then raise Full else 
    let h = { comp = comp;
              arr = Array.copy arr;
              size = ref size } in
      for i = (size / 2) + 1 downto 0 do
        sink comp size h.arr i
      done;
      h;;

(*Compile with 'ocamlc io.mli io.ml heap.mli heap.mli test.ml -o test', to check for problems*)

(*let this be the function that builds the huffman encoding *)
let compress_helper1 in_channel arr =
  let rec compress_helper1_inner in_channel1 =
    (*read everythign character by character*)
    let newchar = input_char in_channel1 in
    let pos = int_of_char newchar in
      arr.(pos) <- arr.(pos) + 1;
      compress_helper1_inner in_channel1
  in
    try compress_helper1_inner in_channel with (*You're gonna need to reach end of file eventually*)
        End_of_file -> ();;
(*ending shit*)
;;
(*This one builds our coding from an array I believe*)
let copier oldarr newarr =
  let placer = ref 0 in
    for x = 0 to 255 do 
      if oldarr.(x) > 0 then 
        begin
          let newchar = char_of_int x in
            newarr.(!placer) <- (Br([newchar], Lf, Lf), oldarr.(x));
            placer := !placer + 1
        end
    done

;;

(*Our comparison function*)
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
(*Our heap is a tuple of a list of chars inside the thing, and a count of it for use in comparison*)

let remove_2min_help heap =
  let (char1, a) = remove_min heap in
  let (char2, b) = remove_min heap in
  let val1 = valtree char1 in
  let val2 = valtree char2 in
    insert heap (Br(val1 @ val2, char1, char2), a+b);;

let remove_2_plz heap =
  try remove_2min_help heap with 
      Empty -> ()
;;
(*This function helps do all the successive 2 remove_mins to make us the right structures*)
let rec remove_2 heap count =
  if count == 0 then () else
    begin 
      remove_2_plz heap;
      remove_2 heap (count -1)
    end 
;;
let rec encodinghelper tree encoding acc=
  match tree with
    |Br ([a], Lf, Lf ) -> acc := (a, (List.rev encoding))::!acc
    |Br (_, l, r) -> encodinghelper l (0:: encoding) acc; encodinghelper r (1::encoding) acc
    |_ -> ()

;;
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
implode [1;0;1;0];;

let implode2 l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> Bytes.set res i  c; imp (i + 1) l in
    imp 0 l;;

(*let rec converter encoder newlist =
  match encoder with
  |(a,b)::t -> converter t ((newlist := (a, implode b)::!newlist);newlist)
  |[] -> ()
  ;; unneccessary *) 
let wow = Br (['c'; 'b'], Br (['c'], Lf, Lf), Br (['b'], Lf, Lf));;
let wow2 = Br (['c'; 'b'; 'a'; 'd'; 'f'; 'e'],
               Br (['c'; 'b'], Br (['c'], Lf, Lf), Br (['b'], Lf, Lf)),
               Br (['a'; 'd'; 'f'; 'e'], Br (['a'], Lf, Lf),
                   Br (['d'; 'f'; 'e'], Br (['d'], Lf, Lf),
                       Br (['f'; 'e'], Br (['f'], Lf, Lf), Br (['e'], Lf, Lf)))));;


(*NICE IT WORKS *)
let encodingbiatch tree newarr =
  let sweet = ref [] in 
    encodinghelper tree [] sweet;  (*sweet is a list of tuples of characters and encodings*)
    (*Now we give a new list to placer which is of char, int list*)
    placer !sweet newarr
;;

let fakearr = Array.make 256 "string" ;;
let sweet = ref [] in 
  encodinghelper wow [] sweet; let newshit = ref [] in
    converter !sweet newshit; !newshit;;
int_of_char 'b';;
fakearr.(98);;
(*This thing writes out tree to our ascii array*)

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
(*NO need to write down branches, when we encounter a number its the same* as a branch later*)
(*Need all the spaces so that this stuff looks pretty, and lets us use input_line*)


let save_tree oc (tr: 'a tree) =
  (*let ch = open_out filename in*)
  save_tree_helper tr oc;
  (*close_out ch*)
  (*What should be our terminator. A long string?*)
  output_char oc '\n';
;;

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
    let pos = int_of_char newchar in
    let code = arr.(pos) in
      (*write the shit*)
      typewritter newout1 code;
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
    compress_helper1 ch1 newarr;
    (*do a bunch of stuff*)
    (*now your newarr is probably good to be used. So let's build the encoding with it*)
    close_in ch1;
    (*Coder gives us the heap we will be using*)
    let coder = hufflepuff newarr in
    let sizer = coder.size in
      remove_2 coder (!sizer);
      (*WE FINALLY HAVE THE ENCODING TREE, NOW TO DO SOMETHING WITH IT *)
      (*Checker is my safety check I guess to see if its the right size. checker is the number of characters total in the file*)
      let (weirdasstree,checker) = coder.arr.(0) in
      (*So lets put this encoding back into another array. SO I can save a little space and use the char_of_int and int_of_char trick once again for quick access*)
      let encodedarr = Array.make 256 [] in
        encodingbiatch weirdasstree encodedarr; (*So this finally works*) (*encodedarr is in a similar fashion to the *)
        let ch2 = open_out outfile in
        let ch3 = open_in infile in
          (*function that does actually writing*)
          writingshit ch3 ch2 encodedarr weirdasstree;
          close_out ch2;
          close_in ch3; 
;;


compress "test.txt" "a.txt";;
compress "newtest.txt" "a1.txt";;
(*NICE*)

(*Compilation code
  ocamlc heap.mli heap.ml io.mli io.ml  compress.ml -o tweeester*)
(*execute with ./tweeester "input file name" "output filename"*)

(*Turns our string into a char list. Form the ocaml sites*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

(*Okay how to do a decompress now *)
let rec loadtreehelper ls tree_stack =
  match ls with 
    |[] -> pop tree_stack
    |h::t -> match h with 
      | "/" -> let l = pop tree_stack in 
          let r = pop (tail tree_stack) in
          let operation = Br ('\\', l, r) in (*Look's familiar*)
            loadtreehelper t (push operation (tail2 tree_stack)) 
      | a -> let newbr = Br ((char_of_int (int_of_string a)), Lf, Lf) in loadtreehelper t (push newbr tree_stack)
;;
let test = explode "//99|98|/97|/100|/102|101|";;

let rec tree_conv ls store acc=
  match ls with
    |[] -> acc 
    |'/'::t -> tree_conv t store ("/"::acc)
    |'|'::t -> tree_conv t [] ((implode2 (List.rev store))::acc)
    |a::t ->  tree_conv t (a::store) acc

;;

let ls2 = tree_conv test [] [];;

loadtreehelper ls2 [];;

let tree_lister ic = 
  let s = input_line ic in
    explode s
;;
let load_tree ic =
  let ls = tree_lister ic in
  let conv = tree_conv ls [] [] in 
    loadtreehelper (conv) [] 
;;
(*This thing will keep grabbing us a char nice until EOF*)

let printtruth io =
  if io then 
    begin
      print_int 1;
      print_newline ()
    end
  else 
    begin
      print_int 0;
      print_newline ()
    end
;;


let rec grabchar input_bits tree =
  match tree with 
    |Br (k, Lf, Lf) -> k
    |Br (k, l, r) -> let nextio = getbit input_bits  in 
          printtruth nextio;
          if nextio (*true = right*) then grabchar input_bits r else
            grabchar input_bits l
    |_ -> '\\'
;;

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
  (*Do a check for if the file is empty la*)
  let ic = open_in infile in
  let oc = open_out outfile in
  let decodetree = load_tree ic in
    typewriter ic oc decodetree;
    close_in ic; 
    close_out oc; 
    decodetree
;;


decompress "a.txt" "shitfile.txt";;
(*Something's fucked up somewhere well*)

decompress "a1.txt" "shitfile1.txt";;
