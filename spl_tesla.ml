(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf
open Spl_syntaxtree
open Spl_cfg
open Spl_utils
open Spl_utils.Printer

type env = {
    statenum: (id, int) Hashtbl.t; (* Map state names to integers *)
    statecalls: (id, string list) Hashtbl.t; (* Statecall name, functions which use it *)
    funcs: (string, (Spl_cfg.env * func)) Hashtbl.t;
    debug: bool;
}

let rec reduce_expr sym ex =
    let rec fn = function
    | And (a,b) -> And ((fn a), (fn b))
    | Or (a,b) -> Or ((fn a), (fn b))
    | Not a -> Not (fn a)
    | Greater (a,b) -> Greater ((fn a), (fn b))
    | Greater_or_equal (a,b) -> Greater_or_equal ((fn a), (fn b))
    | Less (a,b) -> Less ((fn a), (fn b))
    | Less_or_equal (a,b) -> Less_or_equal ((fn a), (fn b))
    | Equals (a,b) -> Equals ((fn a), (fn b))
    | Plus (a,b) -> Plus ((fn a), (fn b))
    | Minus (a,b) -> Minus ((fn a), (fn b))
    | Multiply (a,b) -> Multiply ((fn a), (fn b))
    | Divide (a,b) -> Divide ((fn a), (fn b))
    | Int_constant a as x -> x
    | True -> True | False -> False
    | Identifier i ->
        try 
            let ex = List.assoc i sym in
            let sym = List.remove_assoc i sym in
            reduce_expr sym ex
        with Not_found -> Identifier i
    in Spl_optimiser.fold (fn ex)

(* Convert expression to a string *)
let rec ocaml_string_of_expr ex =
    let rec fn = function
    | And (a,b) -> sprintf "(%s && %s)" 
        (fn a) (fn b)
    | Or (a,b) -> sprintf "(%s || %s)" 
        (fn a) (fn b)
    | Identifier i -> sprintf "%s" i
    | Not e -> sprintf "(! %s)" (fn e)
    | Greater (a,b) -> sprintf "(%s > %s)"
        (fn a) (fn b)
    | Less (a,b) -> sprintf "(%s < %s)" 
        (fn a) (fn b)
    | Greater_or_equal (a,b) -> sprintf "(%s >= %s)" 
        (fn a) (fn b)
    | Less_or_equal (a,b) -> sprintf "(%s <= %s)"  
        (fn a) (fn b)
    | Equals (a,b) -> sprintf "(%s = %s)" 
        (fn a) (fn b)
    | Plus (a,b) -> sprintf "(%s + %s)" 
        (fn a) (fn b)
    | Minus (a,b) -> sprintf "(%s - %s)" 
        (fn a) (fn b)
    | Multiply (a,b) -> sprintf "(%s * %s)"
        (fn a) (fn b)
    | Divide (a,b) -> sprintf "(%s / %s)"
        (fn a) (fn b)
    | Int_constant a -> sprintf "%d" a
    | True -> "1"
    | False -> "0"
    in
    fn ex
    
let ocaml_type_of_arg = function
    | Integer x -> (x, "int",false)
    | Boolean x -> (x, "uint8_t",false)
    | Unknown x -> failwith "type checker invariant failure"

let initial_value_of_arg = function
    | Integer x -> sprintf "%s = 0" x
    | Boolean x -> sprintf "%s = false" x
    | Unknown x -> failwith "type checker invariant failure"

let ocaml_format_of_arg = function
    | Integer x -> (x, "%d")
    | Boolean x -> (x, "%B")
    | Unknown x -> failwith "type checker invariant failure"

(* Run an iterator over only automata functions *)
let export_fun_iter fn genv =
    Hashtbl.iter (fun fname (env, func) ->
        if func.export then fn fname env func
    ) genv.funcs

(* Run a map over only automata functions *)
let export_fun_map fn env =
    Hashtbl.fold (fun fname (env, func) a ->
        if func.export then (fn fname env func) :: a else a
    ) env.funcs []

(* Pretty-print a "module blah = struct end" *)
let pp_module e m fn = 
    e.p (sprintf "module %s = struct" (String.capitalize m));
    indent_fn e fn;
    e.p "end";
    e.nl ()

(* Pretty-print a "module blah:sig end" *)
let pp_module_sig e m fn =
    e.p (sprintf "module %s : sig" (String.capitalize m));
    indent_fn e fn;
    e.p "end";
    e.nl ()

(* Pretty-print a state struct *)
let pp_record_type e tname t =
  e.p (sprintf "struct %s {" tname);
  indent_fn e (fun e ->
    e.p "int state;";
    List.iter (fun (nm,ty,m) ->
      e.p (sprintf "%s %s;" ty nm)
    ) t;
  );
  e.p "};";
  e.nl ()

let pp_state_set e tname t =
  let args = String.concat ", " (List.map
    (fun (nm, ty, _) ->
      sprintf "%s %s" ty nm
    ) t) in
  e.p (sprintf "void %s_add(struct %s *s, uint8_t off, %s)" tname tname args);
  e.p "{";
  indent_fn e (fun e ->
    List.iter (fun (nm, ty, _) ->
      e.p (sprintf "%s[off].%s = %s;" tname nm nm);
    ) t;
  );
  e.p "};"

let emit_bad_statecall e =
  e.p "tesla_bad_transition();"
 
let pp_env env e =
  let max_states = 5 in (* TODO: derive from graph *)
  e.p "#include <stdio.h>";
  e.p "#include <stdint.h>";
  e.p (sprintf "#define MAX_STATES %d" max_states);
  let num_of_state s = Hashtbl.find env.statenum s in
  e.nl ();
  export_fun_iter (fun func_name func_env func_def ->
    let block_list = blocks_of_function func_env env.funcs in
    let registers = list_of_registers func_env in
    pp_record_type e func_name (List.map ocaml_type_of_arg registers);
    e.p (sprintf "struct %s %s_states1[MAX_STATES];" func_name func_name);
    e.p (sprintf "struct %s %s_states2[MAX_STATES];" func_name func_name);
    e.p (sprintf "uint8_t %s_num_states;" func_name);
    e.nl ();
    pp_state_set e func_name (List.map ocaml_type_of_arg registers);
    e.nl ();
    let is_sink_state state = List.length state.edges = 0 in
    let rec tickfn e sym targ =
      e.p (sprintf "/* %s */" targ.label);
      let mfn fn = List.filter (fun x -> fn x.t) targ.edges in
      let msgtrans = mfn (function Message _ -> true |_ -> false) in
      let condtrans = mfn (function Condition _ -> true |_ -> false) in
      let asstrans = mfn (function Assignment _ -> true |_ -> false) in
      let aborttrans = mfn (function |Terminate -> true |_ -> false) in
      if List.length aborttrans > 0 then begin
      emit_bad_statecall e;
      end else begin
        if List.length msgtrans > 0 || (is_sink_state targ) then begin
          let seenh = Hashtbl.create 1 in
          let symbind = String.concat ";"
            (List.fold_left (fun a (k,v) ->
              if not (Hashtbl.mem seenh k) then begin
                Hashtbl.add seenh k ();
                let nex = reduce_expr (List.remove_assoc k sym) v in
                sprintf "%s=%s" k (ocaml_string_of_expr nex) :: a
              end else a
              ) [] sym
            ) in
           let regfn x = e.p
             (sprintf "register_state %d %s h; (* %s *)" (num_of_state targ.label) x targ.label) in
                        match List.length sym with
                        |0 -> regfn "x"
                        |len when len = (List.length registers) ->
                            regfn (sprintf "{%s}" symbind);
                        |_ -> regfn (sprintf "{x with %s}" symbind)
                    end;
                    (* Group common conditionals together *)
                    let condhash = Hashtbl.create 1 in
                    List.iter (fun x -> match x.t with
                        |Condition c -> hashtbl_add_list condhash (reduce_expr sym c) x
                        |_ -> failwith "err") condtrans;
                    (* Partition conditionals into value checks against consts and other *)
                    let condvals = Hashtbl.create 1 in
                    let condother = Hashtbl.create 1 in
                    Hashtbl.iter (fun c xs ->
                        match c with
                        |Equals (Identifier i, Int_constant v) ->
                            hashtbl_add_list condvals i (v,xs)
                        |_ -> Hashtbl.add condother c xs
                    ) condhash;
                    (* Pattern match conditionals *)
                    Hashtbl.iter (fun i vxs ->
                        e.p "begin";
                        e.p (sprintf "match %s with" (ocaml_string_of_expr (Identifier i)));
                        List.iter (fun (v,xs) ->
                        e.p (sprintf "|%d -> begin" v);
                        indent_fn e (fun e -> List.iter (fun x -> tickfn e sym !(x.target)) xs);
                        indent_fn e (fun e -> e.p "end");
                        ) vxs;
                        e.p "|_ -> failwith \"internal error\"";
                        e.p "end;"
                    ) condvals;
                    (* All other conditionals *)
                    Hashtbl.iter (fun c xs ->
                        match c with
                        |True ->
                            e.p (sprintf "begin (* if %s *) " (ocaml_string_of_expr c));
                            indent_fn e (fun e -> List.iter (fun x -> tickfn e sym !(x.target)) xs);
                            e.p "end;";
                        |False -> e.p (sprintf "(* skipped %s *)" (ocaml_string_of_expr c));
                        |c ->
                            e.p (sprintf "if %s then begin" (ocaml_string_of_expr c));
                            indent_fn e (fun e -> List.iter (fun x -> tickfn e sym !(x.target)) xs);
                            e.p "end;"
                    ) condother;
                    (* Assignment blocks *)
                    List.iter (fun x -> match x.t with
                        |Assignment (var,expr) ->
                            e.p (sprintf "begin (* let %s = %s in *) " var (ocaml_string_of_expr (reduce_expr sym expr)));
                            indent_fn e (fun e -> tickfn e ((var,expr)::sym) !(x.target));
                            e.p "end;";
                        |_ -> ()
                    ) asstrans;
                end
            in
            e.p "let rec tick msg (s:states) : states =";
            indent_fn e (fun e ->
                e.p "let h = Hashtbl.create 17 in";
                if List.length registers > 0 then
                    e.p "Hashtbl.iter (fun st xs -> List.iter (fun x -> match st,msg with"
                else 
                    e.p "Hashtbl.iter (fun st x -> match st,msg with";
                List.iter (fun state ->
                    (* pull out all the Message transitions from this state *)
                    let msghash = Hashtbl.create 1 in
                    List.iter (fun x -> match x.t with
                        |Message id -> hashtbl_add_list msghash id x  |_ -> ()) state.edges;
                    Hashtbl.iter (fun id xs ->
                        e.p (sprintf "|%d,`%s (* %s *) ->" (num_of_state state.label) id state.label);
                        List.iter (fun x -> match x.t with
                        |Message id ->
                            indent_fn e (fun e ->
                                (* we got a statecall leading to ... *)
                                let targ_state = !(x.target) in
                                tickfn e [] targ_state;
                            );
                         |_ -> failwith "message"
                        ) xs;
                    ) msghash;
                ) block_list;
                e.p " |_ -> ()";
                if List.length registers > 0 then
                    e.p ") xs) s;"
                else
                    e.p ") s;";
                e.p "if Hashtbl.length h = 0 then raise Bad_statecall else h";
            );
            e.nl();
            e.p "let init () =";
            indent_fn e (fun e ->
                let ist = initial_state_of_env func_env in
                e.p (sprintf "let h = Hashtbl.create 17 in");
                if List.length registers > 0 then begin
                    e.p "let x = {";
                    List.iter (fun a -> e.p (sprintf "%s;" (initial_value_of_arg a))) registers;
                    e.p "} in";  
                end else
                    e.p "let x = () in";
                tickfn e [] ist;
                e.p "h"
            );
            e.nl ();
            e.p "let active = function";
            List.iter (fun state ->
                let msgtrans = List.filter (fun x ->
                    match x.t with |Message _ -> true |_ -> false) state.edges in
                let msglocs = List.map (fun x -> may None (fun l -> Some l.Spl_location.line_num) x.loc) msgtrans in
                let actlocs = List.fold_left (fun a -> function None -> a
                    |Some b -> if List.mem b a then a else b :: a) [] msglocs in
                e.p (sprintf "|%d (* %s *) -> [%s]" (num_of_state state.label) state.label
                    (String.concat ";" (List.map string_of_int actlocs)));
                ()
            ) block_list;
            e.p ("|_ -> failwith \"internal error\"");
        ) env;
    e.p "type s = [";
    hashtbl_iter_indent e (fun e scall _ -> e.p (sprintf "|`%s" scall)) env.statecalls;
    e.p "]";
    e.nl ();

    let rs = export_fun_map (fun fname _ _ ->
        fname, (sprintf "%s.states" (String.capitalize fname)), false) env in
    let rschan = ("__cfn","(unit -> (out_channel * in_channel)) option", true) :: rs in
    indent_fn e (fun e ->
        export_fun_iter (fun fname _ _ ->
            e.p (sprintf "%s = %s.init ();" fname (String.capitalize fname));
        ) env;
    );
    e.p "}";
    e.nl ();
    let cap = String.capitalize in
    e.nl ();
    e.p "let tick s x =";
    indent_fn e (fun e ->
        e.p "let r = match x with"
    );
    hashtbl_iter_indent e (fun e scall funcs ->
        e.p (sprintf "|`%s -> " scall);
        indent_fn e (fun e ->
            if env.debug then e.p "let x' = try";
            e.p "{s with";
            List.iter (fun x ->
                e.p (sprintf "%s = (%s.tick `%s s.%s);" x (cap x) scall x)) funcs;
            e.p "}";
            if env.debug then e.p (sprintf "with Bad_statecall -> (badfn %S s; raise Bad_statecall) in actionfn %S x'; x' " scall scall);
        );
    ) env.statecalls;
    indent_fn e (fun e -> e.p "|_ -> s in r")

(* Populate hashtable with (statecall -> [automaton using it list]) *)
let rec extract_statecalls fname env func_env =
    let add s = hashtbl_add_list env.statecalls (String.capitalize s) fname in
    Hashtbl.iter (fun k state ->
        List.iter (fun edge -> match edge.t with
        |Message id -> add id |_ -> ()
        ) state.edges
    ) func_env.blocks;
    (* Also extract statecalls from any functions called by this automaton *)
    Hashtbl.iter (fun f _ ->
        try let x,_ = Hashtbl.find env.funcs f in
            extract_statecalls fname env x
        with Not_found -> failwith "internal compiler error"
    ) func_env.functions_called

let rec state_to_num counter env fenv =
    Hashtbl.iter (fun sname _ ->
        try let _ = Hashtbl.find env.statenum sname in
            ()
        with Not_found -> begin
            let n = !counter in
            incr counter;
            Hashtbl.add env.statenum sname n
        end
    ) fenv.blocks;
    Hashtbl.iter (fun f _ ->
        try let x,_ = Hashtbl.find env.funcs f in
            state_to_num counter env x
        with Not_found -> failwith "internal compiler error"
    ) fenv.functions_called

let generate_interface envs e =
    let statecalls = Hashtbl.create 1 in
    List.iter (fun env -> Hashtbl.iter 
        (fun k v -> Hashtbl.replace statecalls k ()) env.statecalls) envs;
    let names = List.sort Pervasives.compare 
        (Hashtbl.fold (fun k v a -> k :: a) statecalls []) in
    e.p "type t = [";
    indent_fn e (fun e -> List.iter (fun s -> e.p (sprintf "|`%s" s)) names);
    e.p "]";
    e.nl ();
    e.p "let string_of_statecall (x:t) = match x with";
    indent_fn e (fun e -> List.iter (fun s -> e.p (sprintf "|`%s -> \"%s\"" s s)) names);
    e.nl ()
    
let generate sfile ofiles debug genvs  =
    let counter = ref 0 in
    let schan = open_out (sfile ^ ".spec") in
    let envs = List.map2 (fun genv ofile ->
      let mlout = open_out (ofile ^ ".c") in
      let penvml = init_printer mlout in
      let env = { statenum = Hashtbl.create 1; statecalls = Hashtbl.create 1;
        funcs = genv.functions; debug=debug; } in
      export_fun_iter (fun fname fenv fdef -> extract_statecalls fname env fenv) env;
      export_fun_iter (fun _ fenv _ -> state_to_num counter env fenv) env;
      Hashtbl.iter (fun scall l ->  Hashtbl.replace env.statecalls
        scall (list_unique l)) env.statecalls;
      pp_env env penvml;
      close_out mlout;
      env
    ) genvs ofiles in
    let pifaceout = init_printer schan in
    generate_interface envs pifaceout;
    close_out schan
