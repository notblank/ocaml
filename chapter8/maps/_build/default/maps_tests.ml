open OUnit2
open Maps

let bindings_test name output input = 
    name >:: fun _ -> assert_equal output (AssocListMap.bindings input)

let assoc_tests = let open AssocListMap in [
    bindings_test "empty has no bindings" [] empty;

    let lst = [(3110, "fun")] in
    bindings_test "singleton list has 1 binding" lst (of_list lst);
]

let suite = "test suite for maps" >::: assoc_tests 

let _ = run_test_tt_main suite 
