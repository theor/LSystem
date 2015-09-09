#load "Scripts\load-project.fsx"

open LSystem
open LSystem.LSystem2

let pt2 = PythTree2.system()
pt2.rules

pt2.rules PythTree2.O
pt2.count PythTree2.O

//open LSystem.LSystem3
//open FSharp.Quotations
//open FSharp.Quotations.Patterns
//open FSharp.Quotations.DerivedPatterns
//open FSharp.Quotations.ExprShape
//
//type T = I | O | U of int
//
//let [<ReflectedDefinition>]af x = match x with | I -> true | O -> false | _ -> false
//let a = <@ af @>
//let rec afrec e =
//    match e with
//    | Patterns.Call(None, DerivedPatterns.MethodWithReflectedDefinition body, args) -> printfn "FOUND %A" body; ()
//    | ShapeVar v -> ()
//    | ShapeLambda (v,expr) -> afrec expr
//    | ShapeCombination (o, exprs) ->   List.map afrec exprs |> ignore
//afrec a
//
//let f = <@ function | O -> Res [|I;O|] | I -> Res [|I|] | x -> Res [|x|] @>
//
//let getThen e =
//    match e with
//    | NewUnionCase(a,[NewArray(tArr, arr)]) -> printfn "    %A %A" (a) (List.length arr)
//    | _ -> ()
//
//let rec recognizePlus' quotation =
//    match quotation with
//    | UnionCaseTest x -> printfn "UnionCaseTest %A" quotation; ()
//    | IfThenElse(UnionCaseTest(arg,case), eThen, eElse) -> printfn "%A %A" case eThen; getThen eThen; recognizePlus' eElse
//    | ShapeVar v -> ()
//    | ShapeLambda (v,expr) -> recognizePlus' expr
//    | ShapeCombination (o, exprs) ->  List.map recognizePlus' exprs |> ignore
//let an<'a when 'a : comparison> (f:Quotations.Expr<('a -> RuleResult<'a>)>) = recognizePlus' f
//an f
