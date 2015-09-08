module LSystem

open System.Drawing
open Microsoft.FSharp.Reflection

type Symbol<'a> = 'a

type Rule<'a when 'a:equality> = Symbol<'a> * (Symbol<'a> array)
type Rules<'a when 'a:equality and 'a:comparison> = Map<Symbol<'a>,Symbol<'a> array>
type Axiom<'a when 'a:equality and 'a:comparison> = Symbol<'a> array
type LSystem<'a when 'a:equality and 'a:comparison> =
    { axiom: Axiom<'a>
      rules: Rules<'a> }
module LSystem =
        let create axiom rules =
            { axiom = axiom
              rules = Map.ofList rules }


let inline (%>) pred succ = (pred, succ)
let matchRules rules x =
    let matching = rules |> Map.tryFind x
    match matching with
    | None -> [|x|]
    | Some succ -> succ

let step rules axiom =
    axiom |> Array.collect (matchRules rules)


module Algea =
    type T = A | B
    let system() = LSystem.create [| A |] [ A %> [|A;B|]
                                            B %> [|A|] ]
module Cantor =
    type T = A | B
    let system() =  LSystem.create [| A |]
                                   [ A %> [|A;B;A|]
                                     B %> [|B;B;B|] ]
module PythTree =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let system() =  LSystem.create [| O |]
                                   [ O %> [|I;LB;O;RB;O|]
                                     I %> [|I;I|] ]

(* ALT IMPL 2 *)
type Rule2<'a when 'a:equality and 'a:comparison> = (('a -> bool) * Symbol<'a> array)
type LSystem2<'a when 'a:equality and 'a:comparison> =
    { axiom: Axiom<'a>
      rules: Rule2<'a> list }
module LSystem2 =
        let create axiom rules =
            { axiom = axiom
              rules = rules }
module PythTree2 =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let system() =  LSystem2.create [| O |]
                                   [ (function | O -> true | _ -> false)  %> [|I;LB;O;RB;O|]
                                     (function | I -> true | _ -> false) %> [|I;I|] ]

(* ALT IMPL 3 *)
type RuleResult<'a when 'a:equality and 'a:comparison> = Res of Symbol<'a> array
type Rules3<'a when 'a:equality and 'a:comparison> = ('a -> RuleResult<'a>)
type LSystem3<'a when 'a:equality and 'a:comparison> =
    { axiom: Axiom<'a>
      rules: Rules3<'a> }
module LSystem3 =
        let create axiom rules =
            { axiom = axiom
              rules = rules }
module PythTree3 =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let system() =  LSystem3.create [| O |]
                                   (function | O -> Res [|I;LB;O;RB;O|]
                                             | I -> Res [|I;I|]
                                             | x -> Res [|x|])
    let f = <@ function | O -> Res [|I;LB;O;RB;O|] | I when 4 = 2 -> Res [||] | I -> Res [|I;I|] | x -> Res [|x|] @>
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.DerivedPatterns
    open FSharp.Quotations.ExprShape
    let rec recognizePlus' quotation =
        match quotation with
        | UnionCaseTest x -> printfn "UnionCaseTest %A" quotation; ()
        | ShapeVar v -> ()
        | ShapeLambda (v,expr) -> recognizePlus' expr
        | ShapeCombination (o, exprs) -> List.map recognizePlus' exprs |> ignore
    let an<'a> (f:Quotations.Expr<('a -> 'a[])>) = recognizePlus' f
    an f