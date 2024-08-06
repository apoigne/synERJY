(* mlunit_test.ml *)
open Mlunit;;

u_test_begin "mlunit_test.ml" true;;

let test_ok () = ()
let test_ok2 () = u_assert "u_assert" true
let test_fail () = u_fail "u_fail"
let test_fail2 () = u_assert "u_assert" false
;;


u_case "test_ok"    test_ok   ;;
u_case "test_ok2"   test_ok2  ;;
u_case "test_fail"  test_fail ;;
u_case "test_fail2" test_fail2;;

u_case "test_error" (fun () -> raise (U_Error "U_Error"));;
u_case "test_exn_error" (fun () -> raise Not_found);;
u_exn_case "test_exn_ok" [Not_found] (fun () -> raise Not_found);;

u_info "expect";;
u_info "testcases: 7 / asserts: 1 / errors: 2 / failures: 2";;
(* ---------------------------------------------------------------------- *)
u_test_end "mlunit_test.ml";;
