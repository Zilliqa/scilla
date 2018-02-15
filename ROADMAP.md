Scilla Implementation Roadmap

## Project Structure

* [X] Setup automatic builds
* [X] Produce executable binaries
* [ ] Automatic test suite

## Front-End Compiler

* [ ] Parser for expressions
  * [X] basic expressions
  * [ ] Arithemetics and binders
  * [ ] Constants
* [ ] Parser for statements
  * [ ] Basic control flow
  * [ ] Blockchain interaction
* [ ] Parser for the contract layout
* [ ] Better error reporting (see Menhir docs)
* [ ] Extraction of the parser into Coq (see Menhir docs)
* [ ] Type checker

## Interpreter

* [ ] Interaction with the blockchain

## Verification and Analysis

* [ ] Extraction of programs into Coq
* [ ] Synthesis from Scilla-Coq implementations
