open Ast



(** [unroll_WhileStmt depth l cond loopSpec decClause body postLoop]
    unrolls an AST node of the form
    [WhileStmt (l, cond, loopSpec, decClause, body, postLoop)]
    up to depth [depth] if [depth > 0].
    Calling this function with [depth <= 0] will restore the original loop node
    [WhileStmt (l, cond, loopSpec, decClause, body, postLoop)].

    All parameters' types (except for [depth]) are left unspecified on purpose.
*)
let rec unroll_WhileStmt (depth : int) l cond loopSpec decClause body postLoop 
  : stmt =
  if depth <= 0 then 
    WhileStmt (l, cond, loopSpec, decClause, body, postLoop)
  else
    let unrolledBody = unroll_loops_in_stmt_list depth body in
    let unrolledPostLoop  = unroll_loops_in_stmt_list depth postLoop in
    let newBody = unrolledBody @ unrolledPostLoop in
    let loop = WhileStmt (l, cond, loopSpec, decClause, newBody, []) in
    let get_IfStmt (ifBody : stmt list) = IfStmt (l, cond, ifBody, []) in
    (* expects 'd >= 1'*)
    let rec unroll_simplified_loop (d : int) : stmt =
      if d == 1 
        then get_IfStmt newBody
        else get_IfStmt (newBody @ [unroll_simplified_loop (d-1)])
    in
    unroll_simplified_loop depth
and
(** [unroll_loops_in_stmt depth st] unrolls all non-ghost loops within 
    statement [st] up to depth [depth] if [depth > 0].
    Calling this function with [depth <= 0] returns [st] unaltered.
 *)
unroll_loops_in_stmt (depth : int) (st : stmt) : stmt =
  if depth <= 0 then st
  else
  (
    match st with
    | IfStmt (l, cond, tBranch, fBranch) 
        ->  IfStmt (l, cond, 
                    unroll_loops_in_stmt_list depth tBranch, 
                    unroll_loops_in_stmt_list depth fBranch)
    | WhileStmt (l, cond, loopSpec, decClause, body, postLoop)
        ->  unroll_WhileStmt depth l cond loopSpec decClause body postLoop
    | BlockStmt (l, blockDecls, blockSts, closeBraceLoc, locals_to_free)
        ->  let unrolledBody = unroll_loops_in_stmt_list depth blockSts in
            BlockStmt (l, blockDecls, unrolledBody, closeBraceLoc, 
                        locals_to_free)
    
    
    (* Ignore constructors:
      PureStmt | NonpureStmt | DeclStmt | ExprStmt | SwitchStmt | Assert 
      | Leak | Open | Close 
      | ReturnStmt | PerformActionStmt | SplitFractionStmt | MergeFractionsStmt 
      | CreateBoxStmt | CreateHandleStmt | DisposeBoxStmt | LabelStmt | GotoStmt 
      | NoopStmt | InvariantStmt | ProduceLemmaFunctionPointerChunkStmt
      | DuplicateLemmaFunctionPointerChunkStmt | ProduceFunctionPointerChunkStmt
      | Throw | TryCatch | TryFinally | Break | SuperConstructorCall
      
      Ghost loops are [WhileStmt] instances, nested within a [PureStmt] 
      instance. Therefore, they are ignored, as well.
    *)
    | _ -> st
  )
and
unroll_loops_in_stmt_list (depth : int) (sts : stmt list) : stmt list = 
    List.map (unroll_loops_in_stmt depth) sts
and 
(** Let [rf_decl] be an AST node representing a regular function definition
    that specifies a loop unroll depth [depth > 0], then
    [unroll_loops_in_decl rf_decl] unrolls all non-ghost loops occurring in 
    [rf_decl] up to depth [depth].
    
    Any regular function definition that either does not demand loop unrolling
    or specifies a loop unroll depth [<= 0] remains unaltered.
    The same holds for declarations that do not represent regular function 
    definitions, including fixpoint and lemma function definitions.
 *)
unroll_loops_in_decl (decl : decl) : decl = 
  match decl with
  | Func (l, Regular, type_params', ret_type', name', params', 
          nonghost_callers_only', impl_fct_type', contract', terminates', unroll',
          (Some (body, bodyLoc)), 
          method_binding', visibility') 
      ->  (match unroll' with
          | NoUnrolling -> decl
          | UnrollLoops depth
              -> let unrolledBody = unroll_loops_in_stmt_list depth body
                in
                Func (l, Regular, type_params', ret_type', name', params', 
                      nonghost_callers_only', impl_fct_type', contract', terminates', unroll',
                      (Some (unrolledBody, bodyLoc)), 
                      method_binding', visibility')
          )
  | _ -> decl
and
unroll_loops_in_decl_list (decls : decl list) : decl list =
    List.map unroll_loops_in_decl decls
and unroll_loops_in_package (p : package) : package =
    match p with
    | PackageDecl (l, name, imports, decls)
          -> PackageDecl (l, name, imports, 
                          unroll_loops_in_decl_list decls)
and 
(** [unroll_loops_in_package_list ps] unrolls all non-ghost loops within 
    any regular function definition in [ps] that specifies a loop unroll depth 
    [depth > 0] and returns the transformed package list. 
    The loops are unrolled up to the specified depth [depth].
    
    Any regular function definition that either does not demand loop unrolling
    or specifies a loop unroll depth [<= 0] remains unaltered.
 *)
unroll_loops_in_package_list (ps : package list) : package list =
    List.map unroll_loops_in_package ps