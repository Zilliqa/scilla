Scilla Implementation Roadmap

## Project Structure

* [X] Setup automatic builds
* [X] Produce executable binaries
* [ ] Remove boilerplate from runners via functors
* [ ] Automatic test suite

## Front-End Compiler

* [X] Basic lexer
* [ ] Parser for expressions
  * [ ] Basic expressions
  * [ ] Pattern matching
  * [ ] Constants
* [ ] Parser for statements
  * [ ] Basic control flow
  * [ ] Blockchain interaction
* [ ] Parser for the contract layout
* [ ] Parser for libraries

## Interpreter

* [ ] Executing expressions
  * [ ] CEK-like formalism
* [ ] Implementation of hashing
* [ ] Interaction with the blockchain
* [ ] Serializing state
* [ ] Searialising messages

## Basic analyses

* [ ] Type checker

## Verification and Analysis

* [ ] Extraction of programs into Coq for [Scilla-Coq](https://github.com/ilyasergey/scilla-coq)
* [ ] Synthesis from Scilla-Coq implementations
