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
//    type TT = X | Z | Y of int
//    let m = Map.ofList [(X,1);(Y(2),2)]
//    let un = FSharp.Reflection.FSharpType.GetUnionCases(typeof<TT>)
//    let tagr = FSharp.Reflection.FSharpValue.PreComputeUnionTagReader(typeof<TT>)
//    m.Item X
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