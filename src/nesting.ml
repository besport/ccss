



let prefix_selector prefix s =
    List.flatten  (
      List.map (fun (p1,pl) ->
	List.map (fun (s1,sl) ->
	  p1,(pl@((`Descendant,s1)::sl))
	) s
      ) prefix
    )
let rec convert_rule (selectors,decls) =
  let rules,decls =
    List.fold_left (fun (rules, decls) decl ->
      match decl with
	| `Node (s,d) ->
	  let s = prefix_selector selectors s in
	  (convert_rule (s,d) @ rules,decls)
	| x -> (rules,x::decls)) ([],[]) decls in
  (selectors,List.rev decls)::rules

let convert_statement = function
  | `Rule r -> List.map (fun x -> `Rule x) (convert_rule r)
  | x -> [x]

let convert (maybe_charset, statements) =
  maybe_charset,
  List.flatten (List.map convert_statement statements)
