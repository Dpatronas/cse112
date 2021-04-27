(* $Id: interp.ml,v 1.18 2021-01-29 11:08:27-08 - - $ *)

(*
-- Despina Patronas dpatrona@ucsc.edu
-- Adam Barsness    abarsnes@ucsc.edu
*)

open Absyn

let want_dump = ref false

let source_filename = ref ""

type bnop = float -> float -> float;;
type unop = float -> float;;
type relx = float -> float -> float;;

(* eval_expr-- arg expr type: Absyn.expr, return expr type: float *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number  -> number                    (*returns number*)
    | Memref memref  -> eval_memref memref        (*retrn location*)
    | Unary (unop , e1)  -> Hashtbl.find Tables.unary_fn_table  unop
                           (eval_expr e1)
    | Binary(bnop,e1,e2) -> Hashtbl.find Tables.binary_fn_table bnop
                           (eval_expr e1) (eval_expr e2)

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> eval_STUB "eval_memref Arrayref"
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0  (*if !found ret 0.0*)

and eval_input (memref : Absyn.memref) : string = match memref with
    | Arrayref (ident, expr) -> "eval_memref Arrayref"
    | Variable ident -> ident

and eval_STUB reason = (
   print_string ("(" ^ reason ^ ")"); nan)

and eval_relex relexpr : bool = match relexpr with
    | Relexpr (relx,e1,e2) -> Hashtbl.find Tables.bool_fn_table relx
                             (eval_expr e1) (eval_expr e2)
 
let rec interpret (program : Absyn.program) = match program with
    | [] -> ()                (*empty .. do nothing return unit ()*)
    | firstline::continue  -> match firstline with
       | _, _, None        -> interpret continue
       | _, _, Some stmt   -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim   (ident,  expr) -> interp_STUB "Dim"            continue
    | Let   (memref, expr) -> interp_let   memref   expr   continue
    | Goto  label          -> interp_goto  label
    | If    (expr, label)  -> interp_if    expr     label  continue
    | Print print_list     -> interp_print print_list      continue
    | Input memref_list    -> interp_input memref_list     continue

and interp_let (memref) (expr) (continue) = match memref with
    | Arrayref (ident,expr) ->interp_STUB "Arrayref"       continue
    | Variable ident -> Hashtbl.replace Tables.variable_table
                                       ident (eval_expr expr);
   interpret continue

and interp_goto (label) =
    try let find_label = Hashtbl.find Tables.label_table label
        in interpret find_label
    with Not_found -> exit 0;

and interp_if (expr) (label) (continue) =
    if (eval_relex expr) then interp_goto label;
    interpret continue (*else dont jump*)

and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    (* Recursively call the print_item *)
    in (List.iter print_item print_list; print_newline ());
    interpret continue


and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in (Hashtbl.replace Tables.variable_table
             (eval_input memref) number)
        with End_of_file -> exit 0;
             (* To DO -- Handle EOF properly *) 
    in List.iter input_number memref_list;
    interpret continue

(* 'Not implemented' function = STUBS *)
and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program;
     if !want_dump then Tables.dump_label_table ())

