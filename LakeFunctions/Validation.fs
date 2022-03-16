module LakeFunctions.Validation//此模块无依赖项

///验证吸收塔烟气流速,m/s
let validate_velocity(v) =
    if v < 1.5 || 6.0 < v
    then    "吸收塔烟气流速为[1.5,6.0], 典型值为3.5m/s"
    else    "OK"

///L/G液气比,l/m3湿态,实际氧,实际温度和压力下的体积
///LG=实际的LG*实际流速/3.048 m/s
///header为喷淋层的层数
let validate_lg(lg) =
    if (lg < 5.35 || 20.05 < lg) 
    then    "基于3.048 m/s标准流速的L/G 为[5.35,20.05]。"
    else    "OK"

///烟气入口SO2浓度, SO2/Gas的体积比（干态、实际氧）, ppm [200,4000]
let validate_so2(so2) =
    if (so2 < 200.0 || 4000.0 < so2) 
    then    "吸收塔入口so2浓度为[200,4000]ppm"
    else    "OK"

///Solids=TSS/TF*100
///TSS为未溶解固体量，
///TF浆液总量（包括溶液和未溶解固体）,典型值为15
let validate_solids(solids) =
    if (solids < 10.0 || 20.0 < solids) 
    then    "吸收塔浆液的含固量为[10,20]%, 典型值为15%"
    else    "OK"

///% Inerts/Ash in TSS
let validate_inerts(inerts) =
    let maxValue = 20.0
    if (maxValue < inerts) 
    then    sprintf "Consult Design - %% Inerts/Ash in TSS Out of Range > %f" maxValue
    else    "OK"

let validate_ph(ph) =
    if (ph < 4.8 || 6.0 < ph)
    then "吸收塔浆液的pH值为[4.8,6], 典型值为5.5"
    else "OK"


///SR 钙硫摩尔比
///S  指烟气中的总SO2量
///Ca 指加入石灰石中的总CaCO3量
let validate_sr(sr) =
    if (sr < 1.02 || 1.1 < sr)
    then    "钙硫摩尔比为[1.02,1.1], 典型值为1.03"
    else    "OK"

///grind 石灰石折算到325目的通过率
let validate_grind(grind325) =
    if (grind325 < 65.0 || 95.0 < grind325)
    then    "石灰石325目下的通过率为[65,95]%"
    else    "OK"
    
let validate_levels(levels) =
    if (levels < 1 || 5 < levels)
    then    "喷淋层层数为[1,5]"
    else    "OK"

///吸收区压力降,
///烟气通过吸收塔喷淋层的压力降,kPa,基于3.048 m/s 的标准流速
let validate_dp(dp) =
    if (dp < 0.25 || 1.5 < dp)
    then    "基于3.048 m/s标准流速, 压力降范围为[0.25, 1.5]kPa"
    else    "OK"
        
///喷嘴的流量,m3/hr
let validate_nozzleFlow(flow) =
    if flow < 34.0 || 100.0 < flow
    then  "喷嘴的流量[34,100]m3/hr"
    else  "OK"

///喷嘴的压力降,MPa
let validate_nozzlePressure(pressure) =
    if pressure < 0.05 || 0.14 < pressure
    then  "喷嘴的压力降,mpa[0.05,0.14]"
    else  "OK"

///喷嘴的喷射角度,deg
let validate_nozzleAngle(angle) =
    if angle < 90.0 || 120.0 < angle
    then  "喷嘴的喷射角度为[90,120]度"
    else  "OK"


///喷嘴的密度,个/m2
let validate_nozzleDensity(dens) =
    if dens <= 0.441320327 then //4.1个/100ft2
        "Nozzle Density Low"
    elif dens <= 0.86111391 then //8
        "Nozzle Layout Acceptable"
    elif dens <= 1.076391149 then//10
        "Impingement Expected, No Review Required"
    elif dens <= 1.291669358 then//12
        "Impingement Certian, Process/TDG Review Required"
    elif dens <= 1.508023849 then//14
        "Impingement Certian, Design/Process Review Required"
    else "Not Recommended"


///vel,托盘流速,m/s
let Validate_trayMinVelocity(vel) =
    if vel < 4.8768
    then    "托盘最小流速为4.8768m/s"
    else    "OK"

/// <summary>
/// 
/// </summary>
/// <param name="dp">Pa</param>
let Validate_trayDp(dp) =
    if dp > 747.246
    then    "托盘在3.048m/s时的压力降不大于747.246 Pa"
    else    "OK"
    
///Cl,[0,120000]ppm 氯离子浓度
let validate_cl(cl) =
    if (cl < 0.0 || 120000.0 < cl)
    then    "吸收塔浆液Cl浓度为[0,120000]ppm"
    else    "OK"


///RT Solids residence time ,hours
let validate_rt(rt) =
    if (rt < 10.0 || 30.0 < rt)
    then    "固体停留时间为[10.0,30.0]hour"
    else    "OK"

    

///吸收塔出口SO2浓度不小于10ppmdv
let validate_outletSO2(ppmdv) =
    if ppmdv<10.0 then
        "Consult Design Engineering - Outlet SO2 < 10 ppmdv"
    else
        "OK"

///性能风险：
///margin：余量, % safety
///isInterspacial：指喷淋层是否是对叉式
///eff：脱硫效率, %
let PerformanceRisk(isInterspacial,eff,margin) =
    if isInterspacial then
        if eff >=95.0 then
            if   margin>=20.0 then "Low"
            elif margin>=10.0 then "Med"
            else "High"
        else
            if   margin>=15.0 then "Low"
            elif margin>= 5.0 then "Med"
            else "High"
    else
        if eff >=95.0 then
            if   margin>=15.0 then "Low"
            elif margin>=10.0 then "Med"
            else "High"
        else
            if   margin>=10.0 then "Low"
            elif margin>= 5.0 then "Med"
            else "High"



