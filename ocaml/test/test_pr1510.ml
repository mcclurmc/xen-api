(* Test cases for PR-1510 *)

open OUnit
open Network_utils

(* Example of using OUnitDiff with a String Set *)
module StringDiff =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module OSSet = OUnitDiff.SetMake(StringDiff)

let test_lacp_timeout_prop () =
	let props = Ovs.make_bond_properties "bond_test"
		[ "mode", "lacp" ;
			"lacp-time", "slow" ;
			(* "hashing-algorithm", "src_mac" *) ]
	and correct =
		[ "lacp=active";
			"bond_mode=balance-tcp";
			"other-config:lacp-time=slow" ]
	in

	let propset = OSSet.of_list props in
	let correctset = OSSet.of_list correct in
	OSSet.assert_equal correctset propset

let test_lacp_actor_key () =
	let props = Ovs.make_bond_properties "bond_test"
		[ "mode", "lacp" ; "lacp-actor-key", "slow" ; "hashing-algorithm", "balance-tcp" ]
	and correct = [
		"lacp=active";
		"bond_mode=balance-tcp";
		"other-config:lacp-actor-key=slow"
	] in

	let propset = OSSet.of_list props in
	let correctset = OSSet.of_list correct in
	OSSet.assert_equal correctset propset

let test_lacp_aggregation_key () =
	let props = Ovs.make_bond_properties "bond_test"
		[ "mode", "lacp" ; "lacp-aggregation-key", "42" ]
	and correct = [
		"lacp=active";
		"bond_mode=balance-tcp";
		"other-config:lacp-aggregation-key=42"
	] in

	let propset = OSSet.of_list props in
	let correctset = OSSet.of_list correct in
	OSSet.assert_equal correctset propset

let suite =
	"pr1510_suite" >:::
		[
			"test_lacp_timeout_prop" >:: test_lacp_timeout_prop;
			"test_lacp_actor_key" >:: test_lacp_actor_key;
			"test_lacp_aggregation_key" >:: test_lacp_aggregation_key;
		]
