open Types

let rec tr g f n r e =
  match g with
  | Send(p, q, _, x, t, g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let Rule(b2, l2, e2, r2) = List.assoc q r in
     let env_p = List.assoc p e in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], []))
                (update q (Rule(b2, Fact("In", [Var x])::l2, e2, r2)) r) in
     let e' = update q (x::List.assoc q e) e in
     Rule(b1, l1, e1, Fact("Out", [t])::Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: tr g' f (n+1) r' e'
  | Branch(p, q, _, t, gs) ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let env_p = List.assoc p e in
     let r' = update p (Rule([], [Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)], [], [])) r in
     let rec tr_branch = function
       | [] -> []
       | (pat, g')::gs' ->
          let Rule(b2, l2, e2, r2) = List.assoc q r in
          let r'' = update q (Rule(b1, Fact("In", [pattern_to_term pat])::l2, e2, r2)) r' in
          let e' = update q (binds pat@List.assoc q e) e in
          tr g' f (n+1) r'' e' @ tr_branch gs' in
     Rule(b1, l1, e1, Fact("Out", [t])::Fact(f^"_"^p^"_"^string_of_int n, id_to_var env_p)::r1) :: tr_branch gs
  | Compute(p, New(x, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, Fact("Fr", [Var x])::l1, e1, r1)) r in
     let e' = update p (x::List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e'
  | Compute(p, Let(pat, t, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(LetB(pat, t)::b1, l1, e1, r1)) r in
     let e' = update p (binds pat@List.assoc p e) e in
     tr (Compute(p, letb, g')) f n r' e'
  | Compute(p, Event(name, args, letb), g') ->
     let Rule(b1, l1, e1, r1) = List.assoc p r in
     let r' = update p (Rule(b1, l1, Fact(name, args)::e1, r1)) r in
     tr (Compute(p, letb, g')) f n r' e
  | Compute(p, LetEnd, g') ->
     tr g' f n r e
  | DefGlobal(f', params, g1, g2) ->
     let e' = List.map (fun (p, env_p) ->
                  let rec params_p = function
                      [] -> []
                    | (x, p')::params' -> if p = p' then x::params_p params' else params_p params' in (p, params_p params @ env_p)) e in
     let r' = List.map (fun (p, _) ->
                  (p, Rule([], [Fact(f'^"_"^p^"_"^string_of_int 0, id_to_var (List.assoc p e'))], [], []))) r in
     (* let _ = (f', params)::df in *)
     tr g1 f' 1 r' e' @ tr g2 f n r e
  | CallGlobal(f', args) ->
     let params = List.assoc f' in
     []
  | GlobalEnd -> List.map (fun (p, rule) -> rule) r
