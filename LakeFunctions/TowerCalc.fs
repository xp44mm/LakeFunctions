module LakeFunctions.TowerCalc
open Cuisl


open System
open MaterialProperties

///SO2自然氧化率,%,自然氧化的SO2/浆液吸收的SO2(摩尔比)
///O2,吸收塔入口烟气中O/SO2的摩尔比值
///Cl离子浓度,ppm
let SO2自然氧化率(O2, Cl) = 
    //Multiplier to get TDS from Cl
    //Revised from 2.7 to 1.75
    //TDS溶解的固体
    let tds = 1.75 * Cl / 1e6 * 100.0
    //%
    O2 * (0.185 - 0.005 * tds)
    |> min 75.0

/// <summary>
/// SO2自然氧化率,%
/// </summary>
/// <param name="inletOsStoic">吸收塔入口烟气中O/SO2的摩尔比值</param>
/// <param name="tds">%</param>
let natoxirate inletOsStoic tds =
    inletOsStoic * (0.185 - 0.005 * tds)
    |> min 75.0

///吸收塔入口O2质量消耗,kg/hr;
///so2,o2吸收塔入口的流量,kg/hr;
///tds吸收塔浆液的tds,%;
let absInletO2Consumption so2 o2 tds = 
    //weight -> mole
    let kmolSO2 = so2 / molar "SO2"
    let kmolO2  = o2 / molar  "O2"
    let stoich = kmolO2 / kmolSO2 * 2.0

    //%
    let natoxirate = 
        stoich * (0.185 - 0.005 * tds) 
        |> min 75.0
    natoxirate / 100.0 * kmolSO2 * molar "O"

///强制氧硫摩尔比,% O/S,强制氧化空气中氧气的O原子/强制氧化SO2的摩尔数
///injectDepth,m 相对于设计液位计算的氧化空气管深度
///说明,当氧化空气的饱和温度大于60℃时,需要考虑饱和温度修正系数
let 强制氧硫摩尔比(injectDepth) = 0.76 + 46.5 * 0.3048 / injectDepth
    
///需要注入的最小氧气量
///removal.O2: removal中的O2量,kg/hr
let injectMinO2 so2 o2 tds removalO2 sd =
    let inletO2 = absInletO2Consumption so2 o2 tds
    let stoich = 强制氧硫摩尔比 sd
    (removalO2-inletO2)*stoich


///圆锥体体积
let coneVolume(r, h) = System.Math.PI * square r * h / 3.0

///圆台体体积
let truncateConeVolume(r1, r2, h) = System.Math.PI * h * (square r1 + square r2 + r1 * r2) / 3.0

///吸收塔浆液池体积
///hall,
///holdup,设计液位低于hall的高度;
///dia,吸收塔直径;
let slurryVolume dia hall flare fHeight holdup =
    let designLevel = hall - holdup
    if flare = 0.0 then
        System.Math.PI*square (dia/2.0) * designLevel
    else
        let tankDiameter = dia + 2.0 * flare
        let tankHeight = hall - fHeight
        let tankVolume = System.Math.PI*square (tankDiameter/2.0) * tankHeight
        
        let hdll = designLevel - tankHeight
        let ddll = tankDiameter - 2.0 * flare / fHeight * hdll

        let coneVolume = truncateConeVolume(tankDiameter/2.0,ddll/2.0,hdll)
        coneVolume + tankVolume

/// <summary>
/// 氧化空气抬升体积,m3
/// </summary>
/// <param name="ddll">设计液位直径,m</param>
/// <param name="oxizone">氧化区浆液体积,m</param>
/// <param name="actvol">氧化空气实际体积,m3/hr</param>
let holdupVolume ddll oxizone actvol =
    let vel= actvol / 3600.0 / (System.Math.PI / 4.0 * square ddll)//m/s
    let fac= vel * 4.0
    let airVol= oxizone * fac
    airVol

/// <summary>
/// 氧化空气抬升高度
/// </summary>
/// <param name="dhll">高液位直径</param>
/// <param name="f">f = flare / flareHeight</param>
/// <param name="holdupVol">氧化空气抬升体积</param>
let holdup dhll f holdupVol =
    let hh = dhll/2.0/f //圆锥高度
    let newholdup = cbrt (cubic hh + 3.0/System.Math.PI * holdupVol/square f) - hh
    newholdup


/////反算氧化空气抬升高度
//let holdupRec dllDiameter aboveInjectVolume v hollDiameter f =
//    let vol = holdupVolume dllDiameter aboveInjectVolume v
//    holdup hollDiameter f vol