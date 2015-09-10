module LSystem

open System.Drawing
open Microsoft.FSharp.Reflection

type Symbol<'a> = 'a
type Axiom<'a when 'a:equality and 'a:comparison> = Symbol<'a> array
type Rules<'a when 'a:equality and 'a:comparison> = 'a -> 'a[]
type Counter<'a when 'a:equality and 'a:comparison> = 'a -> int
type LSystem<'a when 'a:equality and 'a:comparison> =
    { axiom: Axiom<'a>
      rules: Rules<'a>
      count: Counter<'a> }
module LSystem =
        let create2 axiom rules count =
            { axiom = axiom
              rules = rules
              count = count }

let inline (%>) pred succ = (pred, succ)
let matchRules rules x =
    let matching = rules |> Map.tryFind x
    match matching with
    | None -> [|x|]
    | Some succ -> succ

let step rules axiom =
    axiom |> Array.Parallel.collect (rules)


//module Algea =
//    type T = A | B
//    let system() = LSystem.create [| A |] [ A %> [|A;B|]
//                                            B %> [|A|] ]
//module Cantor =
//    type T = A | B
//    let system() =  LSystem.create [| A |]
//                                   [ A %> [|A;B;A|]
//                                     B %> [|B;B;B|] ]

module PythTree =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let rules f s = match s with
                    | O -> f [|I;LB;O;RB;O|]
                    | I -> f [|I;I|]
                    | x -> f [|x|]
    let system(f) =  LSystem.create2 [| O |] (rules id) (rules Array.length)

