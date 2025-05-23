
module CopyConstPropFold

open AbSyn

type Expr =
    | Const of int * Pos
    | BoolConst of bool * Pos 
    | Alias of string * Pos

let rec copyConstPropFoldExpr (env: Map<string, Binding>) (expr: Expr) : Expr =
    match expr with 
    
        | Var (x, pos) ->
            match Map.tryFind x env with
           | Some (Const c) -> Const c
           | Some (BoolConst b) -> BoolConst b
           | Some (Alias y) -> Var (y, pos)
           | None -> Var (x, pos)

        | Index (name, ei, t, pos) ->
            let ei' = copyConstPropFoldExpr env ei
            match Map.tryFind name env with
            | Some (Alias y) -> Index (y ei', t, pos)
            | _ -> Index (name, ei', t, pos)


                 
                | Const (v, pos) ->
                    Const (v, pos)
                
                | Let (x, e1, body, pos) ->
                    let e1' = copyConstPropFoldExpr env e1
                    let env' = 
                        match e1' with
                        | Const c -> Map.add x (Const c) env
                        | BoolConst b -> Map.add x (BoolConst b) env 
                        | Var (y, _) -> Map.add x (Alias y) env
                        | _ -> env
                    let body\' = copyConstPropFoldExpr env body 
                    Let (x, e1', body', pos)
                    
        | Plus (e1, e2, pos) ->
            let e1' = copyConstPropFoldExpr env e1
            let e2' = copyConstPropFoldExpr env e2
            match (e1', e2') with
            | Const (c1, _), Const (c2, _) -> Const (c1 + c2, pos)
            | _ -> Plus (e1', e2', pos)

        | Times (e1, e2, pos) ->
            let e1' = copyConstPropFoldExpr env e1
            let e2' = copyConstPropFoldExpr env e2
            match (e1', e2') with 
            | Const (c1, _) Const (c2, _) -> Const (c1 * c2, pos)
            | _ -> Times (e1', e2', pos)
        
        | And (e1, e2, pos) ->
            let e1' = copyConstPropFoldExpr env e1
            let e2' = copyConstPropFoldExpr env e2
            match (e1', e2') with 
            | BoolConst (true, _), x -> x
            | x, BoolConst (true, _) -> x
            | BoolConst (false, _), _ -> BoolConst (false, pos)
            | _, BoolConst (false, _) -> BoolConst (false, pos)
            | _ -> And (e1', e2', pos)
    
        | _ -> expr






