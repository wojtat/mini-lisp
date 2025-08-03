# Mini Lisp

Author: *Vojtěch Tilhon*, e-mail: *vojtech.tilhon@seznam.cz*

## About

This project contains the parser and evaluator of a simplified Lisp-like language.
The project was developed for educational purposes, thus no guarantees can be given.

This Lisp variant implements a few built-in functions,
such as `define`, `lambda`, `car`, `cdr`, `cons`, `empty`, `+`, `-`, and so on.
More advanced functions can be built from these components, however.

## Build

The project is implemented in the zig programming language version 0.13.0.
Make sure you have this specific version of zig installed,
otherwise the code may not compile.

To build the project, run
```
$ zig build
```
in your terminal emulator.

## Usage

The program accepts a list of Lisp files provided as command-line arguments.
All expressions are evaluated and printed to the stdout file in order.

