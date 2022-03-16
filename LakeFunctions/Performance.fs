module LakeFunctions.Performance
open Cuisl

///托盘最大许用烟气流速,
///flux,(m3/hr)/m2
let trayMaxVelocity (flux) = 
    let flux = flux / 100.0
    let a = 0.06039075131 //0.018407101/0.3048;;
    let b = 0.0481358075 //sqrt (0.00000042210495/0.3048) * 0.4090397558 * 100.0
    1.0 / (a + square (b * flux))
    
///基本RTU
let rtu_base (hasTray) = 
    //吸收塔的类型
    if hasTray then 0.95
    else 0.826
    
///入口SO2浓度,ppm [200,4000]
///SO2/Gas的体积比（干态、实际氧）
///？？问题mg/m3与ppm的单位转换。
let rtu_so2 (so2) = 
    let a = 2.1197136
    let b = -0.86406817
    let c = 0.31112427
    let d = -0.066437931
    let e = 0.0057770696
    let milli = so2 / 1000.0
    a + b * milli + c * square milli + d * cubic milli + e * quartic milli
    
///Solids=TSS/TF*100，
///TSS为未溶解固体量，
///TF浆液总量（包括溶液和未溶解固体）,典型值为15
let rtu_solids (solids) = 
    let a = 0.95
    let b = 0.5
    a + b * solids / 100.0
    
let rtu_ph (ph) = 
    let a = -0.872
    let b = 0.312
    a + b * ph
    
///SR 钙硫摩尔比.
///S  指烟气中的总SO2量.
///Ca 指加入石灰石中的总CaCO3量.
let rtu_sr (sr) = 
    let a = 1.5361
    let b = -2.2775
    let c = 1.65
    a + b * sr + c * square sr
    
///L/G液气比,l/m3湿态,实际氧,实际温度和压力下的体积
///LG 实际的LG*实际流速/10[ft/s]，标准流速下的液气比
///levels为托盘上的喷淋层数：Performance!H34
let rtu_lg (lg, levels) = 
    let lg = 
        if levels = 1 then 0.926 * lg
        else lg
        
    let a = 0.3636
    let b = 0.118940252953391
    a + b * lg
    
///passing % 石灰石325目的通过率
let rtu_grind (passing) = 
    let a = 0.261111
    let b = 0.7778
    a + b * passing / 100.0
    
///吸收区压力降,
///烟气通过吸收塔喷淋层的压力降,kPa,基于3.048 m/s 的标准流速
let rtu_dp (dp) = 
    let a = 0.8215
    let b = 0.665242771456789
    let c = 0.193417852744692
    a + b * dp + c * square dp
    
///喷嘴的流量,m3/hr
///喷嘴的压力降,MPa
///喷嘴的喷射角度,deg
let rtu_nozzle (flow, pressure, angle) = 
    [| 
        0.722869
        13.7562498576817 * pressure
        -1.80385472792217 * (flow / 100.0)
        0.4709 * (angle / 100.0)
        4.20186915296637 * (flow / 100.0) * pressure
        -8.83279860334454 * (angle / 100.0) * pressure
        0.86296198846167 * (flow / 100.0) * (angle / 100.0)
        -64.0754950335631 * square pressure
        0.914983344819938 * square (flow / 100.0)
        -0.106 * square (angle / 100.0)
        0.227841469659675 * flow * square pressure
        0.231395418702953 * angle * square pressure
        -4.8921692555745 * square (flow / 100.0) * pressure
        -3.06286797630403 * square (flow / 100.0) * (angle / 1000.0)
        0.948546845088232 * square (angle / 100.0) * pressure
        -7.57293173547996 * square (angle / 100.0) * flow / 10000.0 |]
    |> Array.sum
    
///v:烟气流速,m/s
let rtu_velocity (v, ph) = 
    [| 
        2.508832926
        -2.37905672572178 * (v / 10.0)
        -7.6501034 * ph / 100.0
        -9.56213488815866 * square (v / 10.0)
        1.89812 * square (ph / 100.0) / 1000000.0
        -5.14434153543307 * (v / 10.0) * (ph / 10.0)
        2.75776659192207 * square (v / 10.0) * ph
        -4.4121062992126 * (v / 10.0) * square (ph / 100.0) / 1000000.0 |]
    |> Array.sum
    
///Cl离子RTU,适用于强制氧化，细磨.
///Cl,[0,120000]ppm 氯离子浓度.
///flux,(m3/hr)/m2,循环通量（循环浆液总流量/吸收塔截面积）.
///SO2,ppm,吸收塔入口SO2浓度.
///?RTU_CL(20000,220.3551402,1618,1.03,81.60) .97940226927355.
let rtu_cl (cl, flux, so2) = 
    let a0 = 0.1393
    let a1 = 0.3258 * (cl / 1000.0) / 100.0
    let a2 = -0.5008 * square (cl / 1000.0) / 10000.0
    let a4 = 1.11177005620524 * (flux / 100.0)
    let a6 = -3.834 * so2 / 100000.0
    let a7 = -1.31301761604813 * (flux / 100.0) * (so2 / 1000.0) / 10.0
    let a8 = -1.98629705405911 * (flux / 100.0) * (cl / 1000.0) / 1000.0
    (a0 + a1 + a2 + a4 + a6 + a7 + a8) / (a0 + a4 + a6 + a7) |> min 1.0
    

