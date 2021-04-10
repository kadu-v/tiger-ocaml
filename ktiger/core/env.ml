let builtin_types = [ ("int", Types.INT); ("string", Types.STRING) ]

let builtin_functions = []

let base_tenv =
  List.fold_left
    (fun env (name, ty) -> Symbol.enter (Symbol.symbol name) ty env)
    Symbol.empty builtin_types

let base_venv = Symbol.empty
