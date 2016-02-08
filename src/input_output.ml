open Batteries

type data =
{
  foo: int
}

type sol =
{
  bar: int
}

let parse file: data =
  { foo = 0 }

let out_sol file sol =
  (try Unix.mkdir "outputs" 0o755 with _ -> ());
  let (_, file) = String.split file "inputs/" in  
  let (file, _) = String.split file ".in" in  
  let oc = open_out ("outputs/" ^ file ^ ".out") in
  close_out oc
