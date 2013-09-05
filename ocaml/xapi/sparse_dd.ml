(* Utility program which copies between two block devices, using vhd BATs and efficient zero-scanning
   for performance. *)

open Pervasiveext
open Stringext
open Listext
open Threadext

let ( +* ) = Int64.add
let ( -* ) = Int64.sub
let ( ** ) = Int64.mul

let kib = 1024L
let mib = kib ** kib

let blocksize = 10L ** mib

exception ShortWrite of int (* offset *) * int (* expected *) * int (* actual *)

open Direct_io

type substring = {
  buf: Memory.t;
  offset: int;
  len: int
}

(* Consider a tunable quantum of non-zero'ness such that if we encounter
   a non-zero, we know we're going to incur the penalty of a seek/write
   and we may as well write a sizeable chunk at a time. *)
let roundup x = 
	let quantum = 16384 in
	((x + quantum + quantum - 1) / quantum) * quantum


(** The copying routine has inputs and outputs which both look like a 
    Unix file-descriptor *)
module type IO = sig
	type t
	val op: t -> int64 -> substring -> unit
end
	
(* [partition_into_blocks (s, len) skip f initial] applies contiguous (start, length) pairs to 
   [f] starting at [s] up to maximum length [len] where each pair is as large as possible
   up to [skip]. *)
let partition_into_blocks (s, len) skip f initial = 
	let rec inner offset acc = 
		if offset = s +* len then acc
		else
			let len' = min skip (s +* len -* offset) in
			inner (offset +* len') (f acc (offset, len')) in
	inner s initial

(** Represents a "block allocation table" *)
module Bat = ExtentlistSet.ExtentlistSet(Int64)

(** As we copy we accumulate some simple performance stats *)
type stats = {
	writes: int;  (* total number of writes *)
	bytes: int64; (* total bytes written *)
}

(** Perform the data duplication ("DD") *)
module DD(Input : IO)(Output : IO) = struct
	let fold bat sparse input_op blocksize size f initial = 
		let buf = Memory.alloc (Int64.to_int blocksize) in
		let do_block acc (offset, this_chunk) =
			input_op offset { buf = buf; offset = 0; len = Int64.to_int this_chunk };
			begin (*match sparse with
			| Some zero -> fold_over_nonzeros buf (Int64.to_int this_chunk) rounddown roundup (f offset) acc
			| None ->*) f offset acc { buf = buf; offset = 0; len = Int64.to_int this_chunk }
			end in
		(* For each entry from the BAT, copy it as a sequence of sub-blocks *)
		Bat.fold_left (fun acc b -> partition_into_blocks b blocksize do_block acc) initial bat

	(** [copy progress_cb bat sparse src dst size] copies blocks of data from [src] to [dst]
	    where [bat] represents the allocated / dirty blocks in [src];
	    where if prezeroed is true it means do scan for and skip over blocks of \000
	    while calling [progress_cb] frequently to report the fraction complete
	*)
	let copy progress_cb bat prezeroed src dst blocksize size =
		(* If [prezeroed] then nothing needs wiping; otherwise we wipe not(bat) *)
		let empty = Bat.of_list [] and full = Bat.of_list [0L, size] in
		let bat = Opt.default full bat in
		let bat' = if prezeroed then empty else Bat.difference full bat in
		let sizeof bat = Bat.fold_left (fun total (_, size) -> total +* size) 0L bat in 
		let total_work = sizeof bat +* (sizeof bat') in
		let stats = { writes = 0; bytes = 0L } in
		let with_stats f offset stats substr = 
			f offset stats substr;
			let stats' = { writes = stats.writes + 1; bytes = stats.bytes +* (Int64.of_int substr.len) } in
			progress_cb (Int64.to_float stats'.bytes /. (Int64.to_float total_work));
			stats' in
		let copy offset stats substr = 
			Output.op dst (offset +* (Int64.of_int substr.offset)) substr in
		let input_zero offset { buf = buf; offset = offset; len = len } =
			for i = 0 to len - 1 do
				Memory.set_byte buf (offset + i) '\000'
			done in
		(* Do any necessary pre-zeroing then do the real work *)
		let sparse = if prezeroed then Some '\000' else None in
		fold bat sparse (Input.op src) blocksize size (with_stats copy) 
			(fold bat' sparse input_zero blocksize size (with_stats copy) stats)
end

let blit src srcoff dst dstoff len = 
	(* Printf.printf "[%s](%d) -> [%s](%d) %d\n" "?" srcoff "?" dstoff len; *)
	String.blit src srcoff dst dstoff len

(** A File interface implemented over open Unix files *)
module File_reader = struct
	type t = File.t
	let op stream stream_offset { buf = buf; offset = offset; len = len } = 
		File.seek stream stream_offset;
		File.really_read stream buf offset len 
end
module File_writer = struct
	type t = File.t
	let op stream stream_offset { buf = buf; offset = offset; len = len } = 
		File.seek stream stream_offset;
		File.really_write stream buf offset len
end

module File_copy = DD(File_reader)(File_writer)

(** [file_dd ?progress_cb ?size ?bat prezeroed src dst]
    If [size] is not given, will assume a plain file and will use st_size from Unix.stat.
    If [prezeroed] is false, will first explicitly write zeroes to all blocks not in [bat].
    Will then write blocks from [src] into [dst], using the [bat]. If [prezeroed] will additionally
    scan for zeroes within the allocated blocks.
    If [dst] has the format:
       fd:X
    then data is written directly to file descriptor X in a chunked encoding. Otherwise
    it is written directly to the file referenced by [dst].
 *)     
let file_dd ?(progress_cb = (fun _ -> ())) ?size ?bat prezeroed src dst = 
	let size = match size with
	| None -> (Unix.LargeFile.stat src).Unix.LargeFile.st_size 
	| Some x -> x in
	let ifd = File.openfile src 0o600 in
	if String.startswith "http:" dst || String.startswith "https:" dst then begin
		failwith "networking disabled"
	end else begin
		let ofd = Unix.openfile dst [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600 in
	 	(* Make sure the output file has the right size *)
		let (_: int64) = Unix.LargeFile.lseek ofd (size -* 1L) Unix.SEEK_SET in
		let (_: int) = Unix.write ofd "\000" 0 1 in
		let (_: int64) = Unix.LargeFile.lseek ofd 0L Unix.SEEK_SET in
		Unix.close ofd;
		let ofd = File.openfile dst 0o600 in
		Printf.printf "Copying";
		File_copy.copy progress_cb bat prezeroed ifd ofd blocksize size
	end 

(** [make_random size zero nonzero] returns a string (of size [size]) and a BAT. Blocks not in the BAT
    are guaranteed to be [zero]. Blocks in the BAT are randomly either [zero] or [nonzero]. *)
let make_random size zero nonzero = 
	(* First make a random BAT *)
	let bs = size / 100 in
	let bits = Array.make ((size + bs - 1) / bs) false in
	for i = 0 to Array.length bits - 1 do
		bits.(i) <- Random.bool ()
	done;
	let result = String.create size in
	for i = 0 to size - 1 do
		if bits.(i / bs)
		then result.[i] <- (if Random.float 10. > 1.0 then zero else nonzero)
		else result.[i] <- zero
	done;
	let bat = snd (Array.fold_left (fun (offset, acc) bit -> 
		let offset' = min size (offset + bs) in
		offset', if bit then (offset, offset' - offset) :: acc else acc) (0, []) bits) in
	let bat = Bat.of_list (List.map (fun (x, y) -> Int64.of_int x, Int64.of_int y) bat) in
	result, Some bat

(** [vhd_of_device path] returns (Some vhd) where 'vhd' is the vhd leaf backing a particular device [path] or None.
    [path] may either be a blktap2 device *or* a blkfront device backed by a blktap2 device. If the latter then
    the script must be run in the same domain as blkback. *)
let vhd_of_device path =
	let find_underlying_tapdisk path =
		try 
		(* If we're looking at a xen frontend device, see if the backend
		   is in the same domain. If so check if it looks like a .vhd *)
			let rdev = (Unix.stat path).Unix.st_rdev in
			let major = rdev / 256 and minor = rdev mod 256 in
			let link = Unix.readlink (Printf.sprintf "/sys/dev/block/%d:%d/device" major minor) in
			match List.rev (String.split '/' link) with
			| id :: "xen" :: "devices" :: _ when String.startswith "vbd-" id ->
				let id = int_of_string (String.sub id 4 (String.length id - 4)) in
				let xs = Xs.domain_open () in
				finally
				(fun () ->
					let self = xs.Xs.read "domid" in
					let backend = xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id) in
					let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
					match String.split '/' backend with
					| "" :: "local" :: "domain" :: bedomid :: _ ->
						assert (self = bedomid);
						Some params
					| _ -> raise Not_found
				)
				(fun () -> Xs.close xs)
			| _ -> raise Not_found
		with _ -> None in
	let tapdisk_of_path path =
		try 
			match Tapctl.of_device (Tapctl.create ()) path with
			| _, _, (Some (_, vhd)) -> Some vhd
			| _, _, _ -> raise Not_found
		with Tapctl.Not_blktap ->
			Printf.printf "Device %s is not controlled by blktap\n" path;
			None
		| Tapctl.Not_a_device ->
			Printf.printf "%s is not a device\n" path;
			None
		| _ -> 
			Printf.printf "Device %s has an unknown driver\n" path;
			None in
	begin match find_underlying_tapdisk path with
	| Some path ->
		begin match tapdisk_of_path path with
		| Some vhd -> Some vhd
		| None -> None
		end
	| None -> None
	end

let deref_symlinks path = 
	let rec inner seen_already path = 
		if List.mem path seen_already
		then failwith "Circular symlink";
		let stats = Unix.lstat path in
		if stats.Unix.st_kind = Unix.S_LNK
		then inner (path :: seen_already) (Unix.readlink path)
		else path in
	inner [] path

let with_rdonly_vhd path f = 
	let h = Vhd._open path [ Vhd.Open_rdonly ] in
	finally
	(fun () -> f h)
	(fun () -> Vhd.close h)

let parent_of_vhd vhd = 
	let vhd' = deref_symlinks vhd in
	let parent = with_rdonly_vhd vhd' Vhd.get_parent in
	(* Make path absolute *)
	if String.length parent > 0 && String.startswith "./" parent
	then Filename.concat (Filename.dirname vhd') parent
	else parent

let rec chain_of_vhd vhd = 
	try
		let p = parent_of_vhd vhd in
		vhd :: (chain_of_vhd p)
	with (Failure "Disk is not a differencing disk") -> [ vhd ]

(** Given a vhd filename, return the BAT *)
let bat vhd = 
	with_rdonly_vhd vhd
	(fun h ->
		let b = Vhd.get_bat h in
		let b' = List.map_tr (fun (s, l) -> 2L ** mib ** (Int64.of_int s), 2L ** mib ** (Int64.of_int l)) b in
		Bat.of_list b')

(* Record when the binary started for performance measuring *)
let start = Unix.gettimeofday ()

(* Set to true when we want machine-readable output *)
let machine_readable = ref false 

(* Helper function to print nice progress info *)
let progress_cb =
	let last_percent = ref (-1) in

	function fraction ->
		let new_percent = int_of_float (fraction *. 100.) in
		if !last_percent <> new_percent then begin
			if !machine_readable
			then Printf.printf "Progress: %.0f\n" (fraction *. 100.)
			else Printf.printf "\b\rProgress: %-60s (%d%%)" (String.make (int_of_float (fraction *. 60.)) '#') new_percent;
			flush stdout;
		end;
		last_percent := new_percent

let _ = 
	Stunnel.init_stunnel_path ();
	let base = ref None and src = ref None and dest = ref None and size = ref (-1L) and prezeroed = ref false and test = ref false in
	Arg.parse [ "-base", Arg.String (fun x -> base := Some x), "base disk to search for differences from (default: None)";
		    "-src", Arg.String (fun x -> src := Some x), "source disk";
		    "-dest", Arg.String (fun x -> dest := Some x), "destination disk";
		    "-size", Arg.String (fun x -> size := Int64.of_string x), "number of bytes to copy";
		    "-prezeroed", Arg.Set prezeroed, "assume the destination disk has been prezeroed";
		    "-machine", Arg.Set machine_readable, "emit machine-readable output";
		    "-test", Arg.Set test, "perform some unit tests"; ]
	(fun x -> Printf.fprintf stderr "Warning: ignoring unexpected argument %s\n" x)
	(String.concat "\n" [ "Usage:";
			      Printf.sprintf "%s [-base x] [-prezeroed] <-src y> <-dest z> <-size s>" Sys.argv.(0);
			      "  -- copy <s> bytes from <y> to <z>.";
			      "     <x> and <y> are always interpreted as filenames. If <z> is a URL then the URL";
			      "     is opened and encoded chunks of data are written directly to it";
			      "     otherwise <z> is interpreted as a filename.";
			      "";
			      "     If <-base x> is specified then only copy differences";
			      "     between <x> and <y>. If [-base x] is unspecified and [-prezeroed] is unspecified ";
			      "     then assume the destination must be fully wiped.";
			      "";
			      "Examples:";
			      "";
			      Printf.sprintf "%s -prezeroed      -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "  -- copy 1024 bytes from /dev/xvda to /dev/xvdb assuming that /dev/xvdb is completely";
			      "     full of zeroes so there's no need to explicitly copy runs of zeroes.";
			      "";
			      Printf.sprintf "%s                 -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "";
			      "  -- copy 1024 bytes from /dev/xvda to /dev/xvdb, always explicitly writing zeroes";
			      "     into /dev/xvdb under the assumption that it contains undefined data.";
			      "";
			      Printf.sprintf "%s -base /dev/xvdc -src /dev/xvda -dest /dev/xvdb -size 1024" Sys.argv.(0);
			      "";
			      " -- copy up to 1024 bytes of *differences* between /dev/xvdc and /dev/xvda into";
			      "     into /dev/xvdb under the assumption that /dev/xvdb contains identical data";
			      "     to /dev/xvdb."; ]);
 	if !test then begin
		failwith "testing disabled";
(*
		test_lots_of_strings ();
		exit 0
*)
	end;
	if !src = None || !dest = None || !size = (-1L) then begin
		Printf.fprintf stderr "Must have -src -dest and -size arguments\n";
		exit 1;
	end;
	let empty = Bat.of_list [] in

	Printf.printf "src = %s; dest = %s; base = %s; size = %Ld\n" (Opt.default "None" !src) (Opt.default "None" !dest) (Opt.default "None" !base) !size;
        let size = Some !size in

	(** [chain_of_device device] returns [None] if [device] is None.
	    If device is [Some d] then returns [None] if no vhds were detected or [Some chain] *)
	let chain_of_device device = 
		let flatten = function
		| Some (Some x) -> Some x
		| Some None -> None
		| None -> None in
		let vhd : string option = flatten (Opt.map vhd_of_device device) in
		let chain : string list option = Opt.map chain_of_vhd vhd in
		let option y = Opt.default "None" (Opt.map (fun x -> "Some " ^ x) y) in
		Printf.printf "%s has chain: [ %s ]\n" (option device) (option (Opt.map (String.concat "; ") chain));
		chain in

	let bat : Bat.t option = 
	try
		let src_chain = chain_of_device !src in
		let base_chain = chain_of_device !base in

		(* If the src_chain is None then we have no BAT information *)
		Opt.map
		(fun s ->
			let b = Opt.default [] base_chain in
			(* We need to copy blocks from: (base - src) + (src - base)
			   ie. everything except for blocks from the shared nodes *)
			let unshared = List.set_difference b s @ (List.set_difference s b) in
			List.fold_left Bat.union empty (List.map bat unshared)
		) src_chain
	with e ->
		Printf.printf "Caught exception: %s while calculating BAT. Ignoring all BAT information\n" (Printexc.to_string e);
		None in

	progress_cb 0.;
	let stats = file_dd ~progress_cb ?size ?bat !prezeroed (Opt.unbox !src) (Opt.unbox !dest) in
	Printf.printf "Time: %.2f seconds\n" (Unix.gettimeofday () -. start);
	Printf.printf "\nNumber of writes: %d\n" stats.writes;
	Printf.printf "Number of bytes: %Ld\n" stats.bytes

