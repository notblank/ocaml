open OUnit2
open Maps

let assoc_tests = [

]
let suite = "test suite for maps" >::: assoc_tests 

let _ = run_test_tt_main suite 
