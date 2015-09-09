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
type LSystem2<'a when 'a:equality and 'a:comparison> =
    { axiom: Axiom<'a>
      rules: ('a -> 'a[])
      count: ('a -> int) }
module LSystem2 =
        let create axiom rules count =
            { axiom = axiom
              rules = rules
              count = count }
module PythTree2 =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let rules f s = match s with
                    | O -> f [|I;LB;O;RB;O|]
                    | I -> f [|I;I|]
                    | x -> f [|x|]
    let system(f) =  LSystem2.create [| O |] (rules id) (rules Array.length)

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
