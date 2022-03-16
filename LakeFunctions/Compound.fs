namespace LakeFunctions

///混合物
type Compound(ingredients: seq<string * float>) = 
    let lookup = dict ingredients

    member this.Ingredients = ingredients
    
    member this.Item(nm: string) = 
        if lookup.ContainsKey nm then lookup.[nm]
        else 0.0
    
    ///
    static member (*)(comp: Compound, multiple: float) = 
        let ingredients = 
            comp.Ingredients |> Seq.map(fun (mo, ingr) -> mo, ingr * multiple)
        Compound ingredients
    ///
    static member (/)(comp: Compound, demon: float) = comp * (1.0 / demon)
    
    ///混合
    static member (+)(comp1: Compound, comp2: Compound) = 
        let ingrs = 
            [yield! comp1.Ingredients
             yield! comp2.Ingredients]
            |> Seq.groupBy(fun (mo, _) -> mo)
            |> Seq.map(fun (mo, rows) -> mo, (Seq.sumBy snd rows))
        Compound ingrs

    ///萃取
    static member (-)(a: Compound, b: Compound) = a + (b * (-1.0))


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Compound =
    ///无法转换的成分将被抛弃
    let kg_to_kmol (comp:Compound) =
        comp.Ingredients
        |> Seq.choose(fun(mo,kg)->
            match MaterialProperties.molar mo with
            | 0.0 -> None
            | mu ->Some (mo,kg/mu)
        )
        |> Compound

    ///无法转换的成分将被抛弃
    let kmol_to_kg (comp:Compound) =
        comp.Ingredients
        |> Seq.map(fun(mo,kmol)->
            let kg = kmol*MaterialProperties.molar mo
            mo,kg
        )
        |> Compound
