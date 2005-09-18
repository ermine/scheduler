(*                                                                          *)
(* (c) 22005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)
(* Last modification: 10 Feb 2005                                           *)

type id = unit ref

type event = 
   | Register of (unit -> unit) * float * (unit -> float) * id
   | Unregister of id

type t = {
   reader: Unix.file_descr;
   writer: Unix.file_descr;
   queue: event Queue.t;
   mutex: Mutex.t;
   mutable input_wrote: bool
}

let msgQ = 
   let (f1, f2) = Unix.pipe () in
      { reader = f1;
	writer = f2;
	queue = Queue.create ();
	mutex = Mutex.create ();
	input_wrote = false
      }

let wrap_mutex f =
   try
      Mutex.lock msgQ.mutex;
      try let v = f () in
	 Mutex.unlock msgQ.mutex;
	 v
      with
	 | exc -> Mutex.unlock msgQ.mutex; raise exc
   with exn ->
      Printf.eprintf "Scheduler exc: %s\n" (Printexc.to_string exn);
      flush stdout;
      raise exn

let add_task f start interval =
   wrap_mutex 
      (fun() ->
	  let id = ref () in
	     Queue.add (Register (f, start, interval, id)) msgQ.queue;
	     if msgQ.input_wrote = false then 
		   begin
		      msgQ.input_wrote <- true;
		      let _ = Unix.write msgQ.writer " " 0 1 in ()
		   end
		else ();
	     id
      )

let remove_task id =
   wrap_mutex 
      (fun() ->
	  Queue.add (Unregister id) msgQ.queue;
	  if msgQ.input_wrote = false then
	     begin
		msgQ.input_wrote <- true;
		let _ = Unix.write msgQ.writer " " 0 1 in ()
	     end
	  else ())

type callback = {
   f: unit -> unit;
   time: float;
   interval: unit -> float;
   id: id
}

let rec insert_task tasks task =
   match tasks with
      | [] -> [task]
      | x :: xs ->
	   if x.time <= task.time then
	      x :: insert_task xs task
	   else
	      task :: tasks

let rec scheduler tasks =
   let sleep = match tasks with
      | [] -> 10000000.
      | x :: xs -> 
	   let time = x.time -. Unix.gettimeofday () in
	      if time < 0. then 0.0 else time
   in
   let r, _, _ = Thread.select [msgQ.reader] [] [] sleep in
      match r with
	 | [] ->
	      begin match tasks with
		 | [] -> scheduler tasks;
		 | x :: xs ->
		      let curr_time = Unix.gettimeofday () in
			 if x.time <= curr_time then 
			    begin
			       x.f ();
			       let xinterval = x.interval () in
				  if xinterval <> 0.0 then
				     let new_x = {x with time = 
					   x.time +. xinterval} in
					scheduler (insert_task xs new_x)
				  else
				     scheduler xs
			    end
			 else
			    scheduler tasks
	      end		 
	 | x :: xs -> 
	      let msg = wrap_mutex 
		 (fun _ -> 
		     let msg = Queue.take msgQ.queue in
			if Queue.length msgQ.queue = 0 && 
			   msgQ.input_wrote then begin
			      let s = String.create 1 in
			      let _ = Unix.read msgQ.reader s 0 1 in
				 msgQ.input_wrote <- false
			   end;
			msg
		 )
	      in
		 match msg with
		    | Register (f, start, interval, id) ->
			 let callback = 
			    {f = f; 
			     time = start; 
			     interval = interval;
			     id = id} in
			    scheduler (insert_task tasks callback)
		    | Unregister id ->
			 let rec loop tail =
			    match tail with
			       | [] -> []
			       | x :: xs ->
				    if x.id == id then xs else x :: loop xs
			 in
			    scheduler (loop tasks)

let init () =
   Thread.create scheduler []

(*
let _ =
   let f msg () =
      let tm = Unix.localtime (Unix.time ()) in
	 Printf.printf "[%d:%d] %s\n" tm.Unix.tm_min tm.Unix.tm_sec msg;
	 flush Pervasives.stdout
   in
      init ();
      let count = ref 0 in
	 for i=1 to 100000 do
	    let time = (Unix.gettimeofday ()) +. 2. in
	       incr count;
	       if !count mod 10 = 0 then begin
		  flush stdout;
		  Unix.sleep 1
	       end;
	       ignore (add_task (f ("msg" ^ string_of_int !count)) time
			  (fun() -> 0.));
	 done
*)
