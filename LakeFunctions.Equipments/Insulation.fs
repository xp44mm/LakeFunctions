module LakeFunctions.Insulation

open MathNet.Numerics

///函数返回常年允许最大散热密度，W/m2
///输入介质温度，℃
let 常年最大散热密度 = // annualHeatloss

    let xs,ys = 
        Lake.Insulation.常年允许最大散热密度.DataRecords
        |> Array.map(fun c -> c.介质温度,c.散热密度)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate

///常年允许最高表面温度，℃
///输入介质温度，℃
let annualSurfaceTemperature =

    let xs,ys = 
        Lake.Insulation.常年允许最大散热密度.DataRecords
        |> Array.map(fun c -> c.介质温度,c.表面温度)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate

///函数返回冬季允许最大散热密度，W/m2
///输入介质温度，℃
let winterHeatloss =

    let xs,ys = 
        Lake.Insulation.冬季允许最大散热密度.DataRecords
        |> Array.map(fun c -> c.介质温度,c.冬季)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate

/// <summary>
/// 热导率，W/(m*K)
/// </summary>
/// <param name="material">保温材料的名称</param>
/// <param name="temp">保温材料的平均温度，℃</param>
let conductivity material =
    match material with
    | "硅酸铝" -> fun temp -> 0.035 + 1.65e-4 * temp + 1.242e-7 * pown temp  2
    | "岩棉"   -> fun temp -> 0.037 + 6.81e-5 * temp + 2.999e-7 * pown temp  2
    | "玻璃棉" -> fun temp -> 0.026 + 0.00023 * temp
    | _ -> failwithf "conductivity:%s" material

/// 推荐最高使用温度，℃
/// <param name="material">保温材料的名称</param>
let suggestMaxTemperature material =
    match material with
    | "硅酸铝" -> 650.
    | "岩棉"   -> 350.
    | "玻璃棉" -> 300.
    | _ -> failwithf "suggestMaxTemperature:%s" material

/// 平面保温的外表面温度，℃
/// heatloss 散热密度，W/m2
/// Temperature 温度，℃
/// material 保温材料名称
/// thick 保温厚度, m
let flatOuterTemperature heatloss innerTemperature material thick =
    let cond = conductivity material

    let rec loop times ts =
        let tm = (max 0. innerTemperature + max 0. ts) / 2. //温度不小于0
        let lambda = cond tm // 热导率，W/(m*K)

        let ta = innerTemperature - heatloss * thick / lambda

        if abs (ta  - ts) > 0.01 then
            loop (times + 1 ) ta
        elif times > 999 then
            failwithf "flatOuterTemperature 超出循环次数:%A" (heatloss, innerTemperature, material, thick)
        else
            ta

    loop 0 innerTemperature

/// 管道保温的外表面温度，℃
/// heatloss 散热密度，W/m2
/// Temperature 温度，℃
/// material 保温材料名称
/// thick 保温厚度, m
/// 长度单位为,m
let pipeOuterTemperature heatloss innerTemperature material innerDiameter thick =
    let cond = conductivity material
    let d1 = innerDiameter + 2. * thick

    let rec loop times ts =
        let tn = (max 0. innerTemperature + max 0. ts) / 2. //温度不小于0
        let lambda = cond tn // 热导率，W/(m*K)

        let ta = innerTemperature - 0.5 * d1 * log (d1/innerDiameter) * heatloss / lambda

        if abs (ta  - ts) > 0.01 then
            loop (times + 1 ) ta
        elif times > 999 then
            failwithf "pipeOuterTemperature 超出循环次数:%A" (heatloss, innerTemperature, material, thick)
        else
            ta

    loop 0 innerTemperature
