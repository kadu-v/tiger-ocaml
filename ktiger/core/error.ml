exception Error of string

let error msg = raise (Error msg)
