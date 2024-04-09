open Llvm
exception Error of string

let context = global_context ()
let tl_mod = create_module context "TypeLang"
let builder = builder context
let printf_type = var_arg_function_type (i32_type context) [|pointer_type context|];;

declare_function "printf" printf_type tl_mod;;

let callee =
  match lookup_function "printf" tl_mod with
  | Some callee -> callee
  | None -> raise (Error "unknown function referenced")

let fn = declare_function "main" (function_type (void_type context) [||]) tl_mod
let bb = append_block context "bb" fn

let args = [|build_global_stringptr "Hello World!" "strtmp" builder|];;

dump_value (build_call printf_type callee args "calltmp" builder)