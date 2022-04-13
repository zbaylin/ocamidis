let pp ~sep pp_elt fmt a =
  Array.iteri
    (fun i e ->
      if i <> 0 then Format.fprintf fmt "%a" sep ();
      Format.fprintf fmt "%a" pp_elt e)
    a
