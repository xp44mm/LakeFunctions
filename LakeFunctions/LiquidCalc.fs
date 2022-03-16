module LakeFunctions.LiquidCalc

open System


///浆液固体体积的温度修正系数
///temp-温度,℃
let tempCorrection (temp) = 
    let f = UnitConverter.degF (temp)
    let a = 0.99971963
    let b = 0.73053831 * (1E-07 * f ** 2.5)
    1.0 / (a + b)

///悬浮浆液相对于水的比重
///sg solid
let suspendedSpecificGravity (components:(string*float)[]) =
    let components =
        components 
        |> Array.filter (fun (_,wt) -> wt > 0.0)

    if components.Length = 0 then 1.0 else

    let totalWeight =
        components 
        |> Array.sumBy(fun(_,wt)-> wt)

    let numerator =
        components 
        |> Array.sumBy(fun(form,wt)->
            let sg = MaterialProperties.specificGravity form
            sg * wt
        )

    numerator / totalWeight

/// <summary>根据浆液比重获得浆液密度</summary>
/// <param name="sg">slurry specific gravity,浆液比重,lb/lb</param>
/// <returns>浆液密度,lb/ft^3</returns>
[<Obsolete("英制")>]
let slurryDensity sg = sg * 500.0 / 60.0 * UnitConverter.factor("gal","ft3")


