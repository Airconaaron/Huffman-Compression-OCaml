# Huffman-Compression-OCaml
Homework Assignment for Fundamentals of Programming at Yale-NUS College

## Installation
# MacOS
First install [brew](https://brew.sh/).
Paste the following into terminal
```
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
Next install (OPAM)[https://opam.ocaml.org/]
```
$ brew install opam  
```
# Windows
Coming Soon...


#Compress
To compress compile using the following:
```
ocamlc heap.mli heap.ml io.mli io.ml  compress.ml -o compress
```
Run using the following
```
./compress INPUT_FILE_NAME OUTPUT_FILE_NAME
```


#Decompress

To compress compile using the following:
```
ocamlc heap.mli heap.ml io.mli io.ml  decompress.ml -o decompress
```
Run using the following
```
./decompress INPUT_FILE_NAME OUTPUT_FILE_NAME
```
