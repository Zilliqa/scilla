# libsecp256k1 wrapper for OCaml

[![Build Status](https://travis-ci.org/dakk/secp256k1-ml.svg)](https://travis-ci.org/dakk/secp256k1-ml)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/dakk/bitcoinml/blob/master/LICENSE)


This library wrap the secp256k1 EC(DSA) library into an OCaml library. At the moment
only a subset of functionalities are available:

- Context: create, clone, destroy, randomize
- Elliptic curve: public key creation
- ECDSA: verify, sign, recover


All exchanged data (pubkey, signature, seckey) are represented as hex strings.



## Contributions

You can improve this wrapper by submitting a pull request.

Thanks to all contributors:
- Vincent Bernardoff
- Yoichi Hirai
- Anton Trunov


## Donation

BTC: 13TRVwiqLMveg9aPAmZgcAix5ogKVgpe4T
