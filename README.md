# Unification project


## Presentation

This project is an implementation of a type inference
algorithm that works over terms of a programming language
for functional programming, by using the Herbrand / Robinson algorithm.


## Requirements

### Install OPAM

[OPAM](https://opam.ocaml.org/) is the package manager for OCaml. It
is the recommended way to install the OCaml compiler and OCaml
packages.


## Build

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running the main file

```
$ dune exec 
```

## Testing the code

To test the project, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```
