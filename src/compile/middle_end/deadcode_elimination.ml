
let rec ds_inclusion acc ds ds_keeped =
  match ds_keeped with
  | [] -> acc
  | (x,e)::ds_keeped' ->
      ds_inclusion ((deadcode_elim_ds (ds,e))@acc) ds ds_keeped'

and deadcode_elim_ds (ds,e) =
  let xs = Free_vars.fv e in
  let ds_called_by_e = List.filter (fun (y,_) -> Ast.SMap.mem y xs) ds in
  ds_inclusion ds_called_by_e ds_called_by_e ds


let deadcode_elimination (ds,e) =
  (deadcode_elim_ds (ds,e), e)
