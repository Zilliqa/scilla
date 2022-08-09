# The Scilla changelog

## [UNRELEASED]

This section is to track unreleased features / bugfixes / etc.
Its context will go into the next release.
New pull requests are supposed to add new things to this section.

## Scilla v0.12.0

### Language / interpreter / analyzer features and enhancements

- New `CHAINID` blockchain query (#1070)
- New `_codehash` contract field (#1067)
- New `TIMESTAMP(block_num)` blockchain query (#1067)
- New `alt_bn128_G1_neg` crypto builtin (#1025)
- Dead Code Detector checker (#1016, #1093, #1091, #1101, #1103, #1105, #1113, #1114, #1116, #1121, #1122, #1126, #1127, #1130)
- Parser error messages enhancements (#1031, #1065)
- Initialize libraries, fields and check constraint only once (#1053)
- `pow` builtin works in logarithmic time with adjusted gas charge (#1063)
- Scale gas costs in `eval-runner` (#1037)

### Scilla Command-Line Interface

- Warn if hashing builtins are used with ADTs, maps, messages (#1128)
- Changed out-of-gas error to something less misleading (#1066)
- A command-line flag to disable analyses warnings (#1033)

### Standard library

- G1 point equality utility (#1023)
- CryptoUtils: add `alt_bn128_G1_bmul` and internalize constants (#1026)
- Polynetwork: Change how "m out of n signatures" are checked (#1048)

### Parser / Interpreter / Typechecker bugfixes

- g1point: Bugfix in converting OCaml lit to Scilla lit (#1027)
- Sort maps before serializing / `to_list` (#1039)
- Fixed traversal of dag of imported libraries (#1017)
- Sort libraries when importing them all for pure expression evaluation (#1042)
- Messages sent to libraries should not fail (#1055)
- Remove parser conflicts (#1051)
- Fix pattern match type equivalence (#1079)
- Fix secp256k1 library version (#1081)
- Fix type-cast library names resolution (#1112)
- Fix `bech32_to_bystr20` and `bystr20_to_bech32` builtins: do not throw exceptions (#1119)
- Fix typo in error message produced by `alt_bn128_pairing_product` builtin (#1071)
- Fix hashing collisions for messages with field permutation (##1128)
- Fix `0:0` locations of errors/warnings (#1133)
- Fix location of errors for wrong constructors in pattern-matching in typechecker (#1131)

### Build system, infrastructure and documentation

- Add merging command for parser error messages (#1030)
- Fix OpenSSL version at 1.1 for macOS installation (#1046)
- Fix docker build failure - missing boost dev lib (#1057)
- Restrict OUnit version because of breaking changes in OUnit 2.2.6 (#1075)
- Remove now redundant installation flag for macOS (#1080)
- Resolve the build dependencies of Dockerfile.slim and opam improvements (#1090)
- Ubuntu 20.04 Scilla support (#1094)

### Test suite

- Add ARK contract to test hashes in mutable maps (#1128)
- Test `ecdsa_recover_pk` with `recid` out of range (#1134)
- Add `diff_filter` to `TestSuiteInput` to ignore part of output (#1029)
- Check `gold` file exists before reads (#1136)
- `make gold` does not update JSON gold files if it's only whitespace change (#1110)

### Support for external tools built on top of Scilla

- Scilla-to-OCaml transpiler support (#1028)
- `type_check` and some other aux functions to reuse for scilla-chick project (#1045)
- A bit more structured error messages (#1060)
- Add of_int functions for 256-bit arithmetic (#1062)

### Internal refactors

- Refactor `is_storable_serializable_helper` (#1054)
- Separate `EvalBuiltins` from the evaluation monad (#1043)
