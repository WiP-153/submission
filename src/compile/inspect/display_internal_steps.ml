
type print_mode = Any | Front | Encode | Anf | Let_floating | Lambda_lifting
                | Specialize | Inline | Propagation
                | Matching | MiddleEnd

let string_of_print_mode s =
  match s with
  | "none" -> Any
  | "front" -> Front
  | "encode" -> Encode
  | "anf" -> Anf
  | "float" -> Let_floating
  | "lift" -> Lambda_lifting
  | "spec" -> Specialize
  | "inl" -> Inline
  | "prop" -> Propagation
  | "match" -> Matching
  | "middle-end" -> MiddleEnd
  | _ -> failwith "unknown print mode"

let print_mode = ref Any

let set_print_mode s =
  let pm = string_of_print_mode s in
  print_mode := pm

let display a (ds,e) =
  if a <> !print_mode then () else
  let open Format in
  fprintf std_formatter "@[<v>{debug mode}===========@,@.%a@]" Ast_pprint.pp_prog (ds,e)
