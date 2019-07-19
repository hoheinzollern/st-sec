open Types

let rec tr g f n r e df =
  match g with
  | Send(p, q, {authentic=a; secret=s}, x, t, g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let Rule(b2, l2, e2, r2) = List.assoc q r in
     let env_p = List.assoc p e in
     let (in_f, out_f, extra) = match a, s with
      | false, false -> Fact("In", [Var x]), [Fact("Out", [t])], []
      | false, true -> Fact("Msg_"^f^"_"^string_of_int n, [Var x]), [Fact("Msg_"^f^"_"^string_of_int n, [t])], [Rule([], [Fact("In", [Var "x"])], [], [Fact("Msg_"^f^"_"^string_of_int n, [Var "x"])])]
      | true, false -> Fact("Msg_"^f^"_"^string_of_int n, [Var x]), [Fact("Msg_"^f^"_"^string_of_int n, [t]); Fact("Out", [t])], []
      | true, true -> Fact("Msg_"^f^"_"^string_of_int n, [Var x]), [Fact("Msg_"^f^"_"^string_of_int n, [t])], [] in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], []))
                (update q (Rule(b2, in_f::l2, e2, r2)) r) in
     let e' = update q (x::List.assoc q e) e in
     Rule(b1, l1, e1, out_f@Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: extra @ tr g' f (n+1) r' e' df
  | Branch(p, q, {authentic=a; secret=s}, t, gs) ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let env_p = List.assoc p e in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], [])) r in
     let (out_f, extra) = match a, s with
      | false, false -> [Fact("Out", [t])], []
      | false, true -> [Fact("Msg_"^f^"_"^string_of_int n, [t])], [Rule([], [Fact("In", [Var "x"])], [], [Fact("Msg_"^f^"_"^string_of_int n, [Var "x"])])]
      | true, false -> [Fact("Msg_"^f^"_"^string_of_int n, [t]); Fact("Out", [t])], []
      | true, true -> [Fact("Msg_"^f^"_"^string_of_int n, [t])], [] in
     let rec tr_branch = function
       | [] -> []
       | (pat, g')::gs' ->
          let in_f = match a, s with
          | false, false -> Fact("In", [pattern_to_term pat])
          | _, _ -> Fact("Msg_"^f^"_"^string_of_int n, [pattern_to_term pat]) in
          let Rule(b2, l2, e2, r2) = List.assoc q r in
          let r'' = update q (Rule(b1, in_f::l2, e2, r2)) r' in
          let e' = update q (binds pat@List.assoc q e) e in
          tr g' f (n+1) r'' e' df @ tr_branch gs' in
     Rule(b1, l1, e1, out_f @ Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: extra @ tr_branch gs
  | Compute(p, New(x, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, Fact("Fr", [Var x])::l1, e1, r1)) r in
     let e' = update p (x::List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e' df
  | Compute(p, Let(pat, t, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(LetB(pat, t)::b1, l1, e1, r1)) r in
     let e' = update p (binds pat@List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e' df
  | Compute(p, Event(name, args, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, l1, Fact(name, args)::e1, r1)) r in
     tr (Compute(p, letb, g')) f n r' e df
  | Compute(p, LetEnd, g') ->
     tr g' f n r e df
  | DefGlobal(f', params, g1, g2) ->
     let e' = List.map (fun (p, env_p) ->
                  let rec params_p = function
                      [] -> []
                    | (x, p')::params' -> if p = p' then x::params_p params' else params_p params' in (p, params_p params @ env_p)) e in
     let r' = List.map (fun (p, _) ->
                  (p, Rule([], [Fact(f'^"_"^p^"_"^string_of_int 0, id_to_var (List.assoc p e'))], [], []))) r in
     let df' = (f', params)::df in
     tr g1 f' 1 r' e' df' @ tr g2 f n r e df'
  | CallGlobal(f', args) ->
     let params = List.assoc f' df in
     let pa = List.combine params args in
     List.map (function
           p, Rule(b, l, e, r) ->
           let p_args = pa |> List.filter (fun ((_, p'), _) -> p = p') |> List.map (fun ((_, _), t) -> t) in
           let p_env = [] in
           Rule(b, l, e, Fact(f'^"_"^p^"_"^string_of_int 0, p_args @ p_env)::r)
       ) r
  | GlobalEnd -> List.map (fun (p, rule) -> rule) r
