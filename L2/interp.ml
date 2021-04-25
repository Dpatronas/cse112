(* $Id: interp.ml,v 1.18 2021-01-29 11:08:27-08 - - $ *)

open Absyn

let want_dump = ref false

let source_filename = ref ""

type bnop = float -> float -> float;;
type unop = float -> float;;

(* eval_expr-- arg expr type: Absyn.expr, return expr type: float *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number (*returns number*)
    | Memref memref -> eval_memref memref (*returns location*)
    | Unary(unop,e1)-> Hashtbl.find Tables.unary_fn_table unop
                           (eval_expr e1)
    | Binary(bnop,e1,e2) ->Hashtbl.find Tables.binary_fn_table bnop
                           (eval_expr e1) (eval_expr e2)

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> eval_STUB "eval_memref Arrayref"
    (*look for var in table                              ret value*)
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0  (*if !found ret 0.0*)

and eval_STUB reason = (
   print_string ("(" ^ reason ^ ")");
   nan)

(*
and eval_relex relexpr = match relexpr with
   | Relexpr oper -> try Hashtbl.find Tables.binary_fn_table oper
                     with Not_found -> NAN
*)
    

(* note: efficient pattern matching > hash table lookup *)
let rec interpret (program : Absyn.program) = match program with
    | [] -> () (*empty .. do nothing return unit ()*)
    | firstline::continue -> match firstline with (*3 tuple*)

       (*ignore #,label -- check for stmt*)
       | _, _, None      -> interpret continue         (*none stmt*)
       | _, _, Some stmt -> (interp_stmt stmt continue)(*some stmt*)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident,  expr) -> interp_STUB "Dim (ident, expr)" continue
    | Let (memref, expr) -> interp_STUB "Let (memref, expr)" continue
    | Goto         label -> interp_STUB "Goto label" continue
    | If  (expr, label)  -> interp_STUB "If (expr, label)" continue
    | Print print_list   -> interp_print print_list continue
    | Input memref_list  -> interp_input memref_list continue

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
             (* TO DO -- Store stuffs in sym table *)
             in (print_float number; print_newline ())
        with End_of_file -> (* To DO -- Handle EOF *) 
             (print_string "End_of_file"; print_newline ())
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

