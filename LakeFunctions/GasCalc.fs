module LakeFunctions.GasCalc

open System

///总摩尔数, kmol; 如果某种物质不存在分子量，则排除此种物质。
///components:气体成分，(分子式，质量[kg])
let totalKmol (components:#seq<string*float>) =
    Compound.kg_to_kmol(Compound components).Ingredients
    |> Seq.sumBy snd

///湿空气体积成分，输入：大气压力,MPa;大气温度,℃;相对湿度,%;
let wetAir(press,temp,humidity) =
    //MPa
    let satur_pressure = SaturWater.saturPressure(temp)
        
    //水蒸汽百分比,%
    let vol_H2O = satur_pressure / press * humidity

    //干空气百分比,%
    let vol_dry = 100.0 - vol_H2O

    [
        "H2O", vol_H2O
        "O2" , 0.21 * vol_dry
        "N2" , 0.79 * vol_dry
    ]|> List.toSeq


///空气的绝对湿度,(kg water / kg dry air)
///大气压力,MPa
///大气温度,℃
///相对湿度,%
let specificHumidity(press,temp,relativeHumidity) =
    let air = wetAir(press,temp,relativeHumidity) |> Compound |> Compound.kmol_to_kg
    air.["H2O"]/(air.["O2"]+air.["N2"])
    
///reagent:"Lime"/"Limestone";
///components,参与反应的污染物质量,kg
let reactHeat(reagent, removals:#seq<string*float>) =
    let isLime = String.Equals(reagent, "Lime",StringComparison.OrdinalIgnoreCase)
    let removals = removals |> Map.ofSeq |> Map.map(fun nm wt -> wt/MaterialProperties.molar nm)

    [
        (*SO2 -> SO3--*) 613135.356846408 , 58619.3915143368 , (removals.["SO2"] - removals.["O2"]*2.0)
        (*SO2 -> SO4--*) 454223.468344529 , 340777.497902512 , removals.["O2"]*2.0
        (*HCl         *) 278294.37060071  , 164852.619294465 , removals.["HCl"]
        (*HF          *) 152701.841720641 , 90455.6514183127 , removals.["HF"]
        (*SO3         *) 766260.800986912 , 73259.0958808352 , removals.["SO3"]
    ]
    |> List.sumBy(fun(lime,limestone,kmol)-> 
        //cm 摩尔反应热，kJ/kmol
        let cm = if isLime then lime else limestone
        cm*kmol
    )

///返回给定成分的气体在给定温度下的总焓(kJ).
///成分的每一项对应成分的分子式，和成分的质量(kg).
///温度单位是oC。
let gasEnthalpy (components:#seq<string*float>) temp =
    let k = temp + 273.15

    components 
    |> Seq.filter (fun (_,kg) -> kg > 0.0)
    |> Seq.map(fun(form,kg)->
        let mw = MaterialProperties.molar form
        let kmol = kg/mw
        let moleEnthalpy = IdealGas.moleEnthalpy form k

        kmol * moleEnthalpy
    )
    |> Seq.sum



/// <summary>
/// 返回烟气温度,℃
/// </summary>
/// <param name="components">烟气成分:分子式,质量kg</param>
/// <param name="enthalpy">总焓, kJ</param>
let gasTemperature (components:#seq<string*float>) enthalpy =
    let gasEnth = gasEnthalpy components
    let rec bisect tlow thigh =
        let t = (tlow+thigh)/2.0
        if thigh - tlow < 0.0001 then t else
        if gasEnth t > enthalpy then
            bisect tlow t
        else
            bisect t thigh
    bisect 0.0 1027.0



///混合气体1和气体2后的温度，假设没有水分析出。
///温度单位是℃。
let mixTemprature(components1:#seq<string*float>, components2:#seq<string*float>, t1, t2) =
    if t1 = t2 then t1 else

    let totalweight components =         
        components 
        |> Seq.filter (fun (_,kg) -> kg > 0.0)
        |> Seq.sumBy(fun (_,kg) -> kg)

    if totalweight components1 = 0.0 then t2 else
    if totalweight components2 = 0.0 then t1 else

    let h1 = gasEnthalpy components1 t1
    let h2 = gasEnthalpy components2 t2

    let components3 =
        [yield! components1; yield! components2]
        |> Seq.filter (fun (_,kg) -> kg > 0.0)
        |> Seq.groupBy(fst)
        |> Seq.map(fun(nm,comps)->
            let v = comps |> Seq.sumBy(snd)
            nm,v
        )

    let enthalpy = gasEnthalpy components3
    let h3 = h1+h2

    //用两分法求最终解
    let rec bisect tlow thigh =
        let t = (tlow+thigh)/2.0
        if thigh - tlow < 0.0001 then t else

        let h = enthalpy t
        if h > h3 then
            bisect tlow t
        else
            bisect t thigh

    let tlow,thigh = if t1<t2 then t1,t2 else t2,t1
    bisect tlow thigh

///当气体从温度t0变化到t1时，所获得的热量(kJ)
///t0, t1初态温度和终态温度，℃
let gainHeat (components:#seq<string*float>, t0, t1) =
    let h0 = gasEnthalpy components t0
    let h1 = gasEnthalpy components t1

    h1-h0

///加热(heat>0)或冷却(heat<0)气体，返回终态的温度，℃
let heating (components:#seq<string*float>, t0, heat) =
    if heat = 0.0 then t0 else

    let enthalpy = gasEnthalpy components

    let h0 = enthalpy t0
    let h1 = h0 + heat
        
    //找到解两侧的点
    //t->tt->hh
    let rec brute t =
        let dt = float(sign heat) * 10.0
        let tt = t + dt
        let hh = enthalpy tt

        if hh = h1 then
            tt,tt
        elif sign (h1 - hh) * sign dt > 0 then
            //tt位于t和解之间
            brute tt
        elif dt > 0.0 then 
            t,tt 
        else 
            tt,t

    //用两分法求最终解
    let rec bisect tlow thigh =
        let t = (tlow+thigh)/2.0

        if thigh - tlow < 0.001 then t else

        let h = enthalpy t
        if h > h1 then
            bisect tlow t
        else
            bisect t thigh
            
    let tlow,thigh = brute t0
    bisect tlow thigh

///返回饱和气体的水蒸气质量,kg；
///出口气体成分，(分子式，质量(kg))；
///温度输入oC；
///压力是绝对压力，MPa；
let gasSaturWaterMass(components:#seq<string*float>) press temp =
    //饱和湿度,%
    let humidity = SaturWater.saturPressure temp / press * 100.0

    //干气体的摩尔数(kmol)
    let dryKmol =
        components 
        |> Seq.filter (fun (form,kg) -> kg > 0.0 && form<> "H2O")
        |> totalKmol

    let kmolSaturWater = dryKmol / (100.0 - humidity) * humidity
    kmolSaturWater * MaterialProperties.molar "H2O"

///返回饱和温度，oC；
///气体成分：(分子式，质量(kg))；
///压力是绝对压力，MPa；
let gasSaturTemprature (components:#seq<string*float>, press) =
    let components =
        components 
        |> Seq.filter (fun (form,kg) -> kg > 0.0)
        |> Seq.map(fun(form,kg)->
            let mw = MaterialProperties.molar form
            let kmol = kg/mw
            form,kmol
        )
    let waterKmol = components |> Seq.find(fun (form,_)->form = "H2O") |> snd
    let totalKmol = components |> Seq.sumBy(snd)

    //水的分压力
    let pp = waterKmol /totalKmol * press
    SaturWater.saturTemprature pp

///蒸发冷却过程，返回气体的饱和温度，℃；
///components:出口气体成分，(分子式，质量(kg))；
///press: 出口绝对压力，MPa；
///temp:入口温度，℃；
///heat：与外界的热交换，吸热为正，kJ
let evaporativeCooling(components:#seq<string*float>, press, temp, heat) =
    //入口水量
    let water = 
        components 
        |> Seq.find(fst >> (=) "H2O")
        |> snd

    //汽化潜热,kJ/kg
    let latentHeat = SaturWater.latentHeat press

    //偏函数2个
    let getEnth = gasEnthalpy components
    let getSaturWater = gasSaturWaterMass components press

    //入口水量等于输入温度下的饱和水量, kg
    if water = getSaturWater temp then temp else

    //入口空气的总焓, kJ
    let ein = getEnth temp
        
    let rec bisect tlow thigh =
        let t = (tlow + thigh) / 2.0
            
        if thigh - tlow < 0.001 then t else
                        
        //出口气体的焓，不包括蒸发水焓, kJ
        let eout = getEnth t

        //当前循环温度下的饱和水量, kg
        let sw = getSaturWater t

        //蒸发水热量, kJ
        let lheat = (sw - water) * latentHeat
            
        if eout + lheat > ein + heat then
            bisect tlow t
        else
            bisect t thigh

    bisect 0.0 temp
///酸露点, ℃；
///SO3浓度,ppmdv；
///moist, 水的体积含量, vol %；
let dewPoint(so3, moist) = 88.4 + 18.7 * log10(so3) + 27.6 * log10(moist)


