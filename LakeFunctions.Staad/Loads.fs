module LakeFunctions.Ribs.Loads
open LakeFunctions
open System

///self=I/len,计算面板；neighbor=I/len,邻面板
let ribBeta self neighbor = (1.0 + (self / neighbor)**3.0) / (1.0 + self / neighbor)

///这是用于计算保温荷载的，偏于保守的保温厚度,mm
///temp,温度,℃
let insulaThick temp =
    match temp with
    | t when t <= 50. -> 100.0
    | t when t <= 150. -> 120.0
    | t when t <= 200. -> 140.0
    | t when t <= 250. -> 160.0
    | t when t <= 350. -> 200.0
    | t when t <= 400. -> 250.0
    | _ -> Double.NaN

///保温密度,kN/m3;
///temp,温度,℃
let InsulationDensity temp =
    if temp <= 350. then 1.2 //岩棉板
    elif temp <= 650. then 1.5 //硅酸铝岩棉复合
    else Double.NaN

//保温荷载,kPa
let InsulationWeight temp =
    //保温密度,kN/m3
    let rho = InsulationDensity temp
    //保温厚度,mm
    let insulaThick = insulaThick temp
    //外护板0.1kPa(kN/m2),其他支撑件暂时按0.05kPa考虑，还没有验证。
    let casing = 0.15
    rho * insulaThick / 1000. + casing