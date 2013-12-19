(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Fun
open OUnit

let base_suite =
	"base_suite" >:::
		[
			Test_basic.test;
			Test_pool_db_backup.test;
			Test_xapi_db_upgrade.test;
			Test_ca91480.test;
			Test_vdi_allowed_operations.test;
			Test_pool_apply_edition.test;
			Test_pool_license.test;
			Test_platformdata.test;
			Test_sm_features.test;
			Test_xenopsd_metadata.test;
			Test_ca121350.test;
		]

let handlers = [
	"get_services", Http_svr.FdIO Xapi_services.get_handler;
	"post_services", Http_svr.FdIO Xapi_services.post_handler;
	"put_services", Http_svr.FdIO Xapi_services.put_handler;
	"post_root", Http_svr.BufIO (Api_server.callback false);
	"post_json", Http_svr.BufIO (Api_server.callback true);
	"post_jsonrpc", Http_svr.BufIO Api_server.jsoncallback;
	"post_remote_db_access",
	  Http_svr.BufIO Db_remote_cache_access_v1.handler;
	"post_remote_db_access_v2",
	  Http_svr.BufIO Db_remote_cache_access_v2.handler;
]

let start_server handlers =
	List.iter Xapi_http.add_handler handlers;
	Xapi.listen_unix_socket ()

let harness_init () =
	Printexc.record_backtrace true;
	Pool_role_shared.set_pool_role_for_test ();
	Xapi.register_callback_fns ();
	start_server handlers

let harness_destroy () = ()

let _ =
	harness_init ();
	run_test_tt_main base_suite |> ignore;
	harness_destroy ();
