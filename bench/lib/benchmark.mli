open Core
open Core_bench

val measure_all
  :  Test.Basic_test.t list
  -> run_config:Run_config.t
  -> quota:Quota.t
  -> Measurement.t list
