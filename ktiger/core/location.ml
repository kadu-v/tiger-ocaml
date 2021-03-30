type location = {
  loc_fname : string;
  loc_lnum : int;
  loc_bol : int;
  loc_cnum : int;
}
[@@deriving show]

let equal_location _ _ = true

let to_location (pos : Lexing.position) : location =
  {
    loc_fname = pos.pos_fname;
    loc_lnum = pos.pos_lnum;
    loc_bol = pos.pos_bol;
    loc_cnum = pos.pos_cnum;
  }
