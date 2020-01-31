open Core

(** Send msg via output channel [oc] with a delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Out_channel.output_string oc msg';
  Out_channel.flush oc
