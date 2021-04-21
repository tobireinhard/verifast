open Ast



(** [unroll_loops_in_stmt depth st] unrolls all non-ghost loops within 
    statement [st] up to depth [depth] if [depth > 0].
    Calling this function with [depth <= 0] returns [st] unaltered.
 *)
val unroll_loops_in_stmt : int -> stmt -> stmt


(** Let [rf_decl] be an AST node representing a regular function definition
    that specifies a loop unroll depth [depth > 0], then
    [unroll_loops_in_decl rf_decl] unrolls all non-ghost loops occurring in 
    [rf_decl] up to depth [depth].
    
    Any regular function definition that either does not demand loop unrolling
    or specifies a loop unroll depth [<= 0] remains unaltered.
    The same holds for declarations that do not represent regular function 
    definitions, including fixpoint and lemma function definitions.
 *)
val unroll_loops_in_decl : decl -> decl


(** [unroll_loops_in_package_list ps] unrolls all non-ghost loops within 
    any regular function definition in [ps] that specifies a loop unroll depth 
    [depth > 0] and returns the transformed package list. 
    The loops are unrolled up to the specified depth [depth].
    
    Any regular function definition that either does not demand loop unrolling
    or specifies a loop unroll depth [<= 0] remains unaltered.
 *)
val unroll_loops_in_package_list : package list -> package list