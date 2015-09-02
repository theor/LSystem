module LSystem

open System.Drawing

type Symbol<'a> = 'a

type Rule<'a when 'a:equality> = Symbol<'a> * (Symbol<'a> array)
type LSystem<'a when 'a:equality> =
    { axiom: Symbol<'a> array
      rules: Rule<'a> list }
let inline (%>) pred succ = (pred, succ)
let matchRules rules x =
    let matching = rules |> List.tryFind (fun (pred,_) -> pred = x)
    match matching with
    | None -> [|x|]
    | Some (pred,succ) -> succ

let step rules axiom =
    axiom |> Array.collect (matchRules rules)

module Algea =
    type T = A | B
    let system() = { axiom = [| A |]
                     rules = [ A %> [|A;B|]
                               B %> [|A|] ] }
module Cantor =
    type T = A | B
    let system() = { axiom = [| A |]
                     rules = [ A %> [|A;B;A|]
                               B %> [|B;B;B|] ] }
module PythTree =
    type State = {pos:PointF; angle:int}
    let init x y = {pos=PointF(x,y); angle=0}
    type T = O | I | LB | RB
    let system() = { axiom = [| O |]
                     rules = [ O %> [|I;LB;O;RB;O|]
                               I %> [|I;I|] ] }