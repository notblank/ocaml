open OUnit2
open Sort_desc

let tests = "test suite for sort_desc" >:::[
    "empty" >:: (fun _ -> assert_equal [] (sort_desc []));
    "first 3" >:: (fun _ -> assert_equal [3; 2; 1] (sort_desc [3; 1; 2]));
    "negative" >:: (fun _ -> assert_equal [-1; -3] (sort_desc [-3; -1]));
]

let _ = run_test_tt_main tests
