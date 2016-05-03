open Printf;;
open Secp256k1;;

let ctx = Secp256k1.context_create Secp256k1.CONTEXT_VERIFY in
let ctx2 = Secp256k1.context_randomize ctx "A441B15FE9A3CF56661190A0B93B9DEC7D04127288CC87250967CF3B52894D11" in
let pk = Secp256k1.ec_pubkey_create ctx "ciaomondo" in
match pk with
| None -> Printf.printf "Noneee\n%!"
| Some(pk) -> Printf.printf "Created\n%!"
;;