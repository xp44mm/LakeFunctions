module LakeFunctions.IdealGas

///理想气体
let private lookaside  =
    Lake.Thermal.理想气体摩尔热容.DataRecords
    |> Array.map(fun c -> c.分子式,(c.a0,c.a1,c.a2,c.a3))
    |> Map.ofArray

    //use db = new LakeEf.LakeContext()
    //query {
    //    for c in db.理想气体摩尔热容 do
    //        select (c.分子式,(c.A0,c.A1,c.A2,c.A3))
    //        }
    //|> Map.ofSeq
       
///返回理想气体摩尔比焓(kJ/kmol).
///第一个参数输入理想气体的分子式.
///第二个参数输入绝对温度，K.
let moleEnthalpy = 
    //拟合公式
    let fit(a0, a1, a2, a3) = 
        fun k -> a0 * k + a1 * (0.1 * k)**2.0 / 2.0 + a2 * (0.1 * k)**3.0 / 300.0 + a3 * (0.01 * k)**4.0 / 40.0
    let di = lookaside |> Map.map(fun _ (a0, a1, a2, a3) -> fit(a0, a1, a2, a3))
    function 
    | "HF" -> di.["HCl"]
    | frm -> di.[frm]

///返回某物质给定温度(℃)下的焓(kJ).
let enthalpy temp form kg = 
    if kg > 0.0 then 
        let kmol = kg / MaterialProperties.molar form
        kmol * moleEnthalpy form (temp + 273.15)
    else 0.0

/// <summary>
/// 气体实际体积对标准体积v0的压力修正:vactual = v0 * pcorrect p
/// </summary>
/// <param name="p">实际绝对压力,Pa</param>
let pcorrect p = 101325.0 / p

/// <summary>
/// 气体实际体积对标准体积v0的温度修正:vactual = v0 * tcorrect t
/// </summary>
/// <param name="t">实际温度，℃</param>
let tcorrect t = (t + 273.15) / 273.15

///ActualVolumeFactor,气体从标准体积v0转到实际体积vactual的因子，即 vactual = v0 * volumeCorrect
///pres,绝对压力,Pa；
///temp,温度，℃；
let volumeCorrect pres temp = pcorrect pres * tcorrect temp
