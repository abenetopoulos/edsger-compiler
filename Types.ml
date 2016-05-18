type typ = TYPE_void
         | TYPE_int
         | TYPE_double
         | TYPE_char
         | TYPE_bool
         | TYPE_pointer of typ * int
         | TYPE_none
         | TYPE_proc

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_double         -> 10
   | TYPE_char           -> 1
   | TYPE_bool           -> 1
   | TYPE_pointer _      -> 2
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   (*| TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2*)
   | TYPE_pointer (typ1, cnt1), TYPE_pointer (typ2, cnt2) -> cnt1 = cnt2 && equalType typ1 typ2
   | _                                            -> t1 = t2

