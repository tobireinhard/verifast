open Ast

let maxDepth = 5
let maxDepthStr = string_of_int maxDepth


let rec unroll_loops_in_stmt (depth : int) (st : stmt) : stmt =
  if depth <= 0 then st
  else
  (
    match st with
    | IfStmt (l, cond, tBranch, fBranch) 
        ->  IfStmt (l, cond, 
                    unroll_loops_in_stmt_list depth tBranch, 
                    unroll_loops_in_stmt_list depth fBranch)
    | WhileStmt (l, cond, loopSpec, decClause, body, postLoop)
        ->  let unrolledBody = unroll_loops_in_stmt_list maxDepth body in
            let unrolledPostLoop = unroll_loops_in_stmt_list maxDepth postLoop in
            let newBody = unrolledBody @ unrolledPostLoop in
            let loop = WhileStmt (l, cond, loopSpec, decClause, newBody, []) in
            let unrolled_loop_tail = unroll_loops_in_stmt (depth - 1) loop in
            IfStmt (l, cond, newBody @ [unrolled_loop_tail], [])
    | BlockStmt (l, blockDecls, blockSts, closeBraceLoc, locals_to_free)
        ->  BlockStmt (l,
                      unroll_loops_in_decl_list depth blockDecls, 
                      unroll_loops_in_stmt_list depth blockSts, 
                      closeBraceLoc, locals_to_free)
    (* PerformActionStmt : What does it represent? *)

    (* PureStmt | NonpureStmt | DeclStmt | ExprStmt | SwitchStmt | Assert 
      | Leak | Open | Close 
      | ReturnStmt | PerformActi    onStmt | SplitFractionStmt | MergeFractionsStmt 
      | CreateBoxStmt | CreateHandleStmt | DisposeBoxStmt | LabelStmt | GotoStmt 
      | NoopStmt | InvariantStmt | ProduceLemmaFunctionPointerChunkStmt
      | DuplicateLemmaFunctionPointerChunkStmt | ProduceFunctionPointerChunkStmt
      | Throw | TryCatch | TryFinally | Break | SuperConstructorCall
    *)
    | _ -> st
  )
and unroll_loops_in_stmt_list (depth : int) (sts : stmt list) : stmt list = 
    List.map (unroll_loops_in_stmt depth) sts
and unroll_loops_in_decl (depth : int) (decl : decl) : decl = 
  match decl with
  | Func (l, func_kind', type_params', ret_type', name', params', 
          nonghost_callers_only', impl_fct_type', contract', terminates', unroll',
          (Some (body, bodyLoc)), 
          method_binding', visibility') 
      ->  let unrolledBody = unroll_loops_in_stmt_list depth body
          in
          Func (l, func_kind', type_params', ret_type', name', params', 
                nonghost_callers_only', impl_fct_type', contract', terminates', unroll',
                (Some (unrolledBody, bodyLoc)), 
                method_binding', visibility') 
  | _ -> decl
and unroll_loops_in_decl_list (depth : int) (decls : decl list) : decl list =
    List.map (unroll_loops_in_decl depth) decls
and unroll_loops_in_package (depth : int) (p : package) : package =
    match p with
    | PackageDecl (l, name, imports, decls)
          -> PackageDecl (l, name, imports, 
                          unroll_loops_in_decl_list depth decls)
and unroll_loops_in_package_list (depth : int) (ps : package list) : package list =
    List.map (unroll_loops_in_package depth) ps