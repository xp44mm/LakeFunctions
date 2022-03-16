module LakeFunctions.Atmosphere

///airVol:湿空气体积百分比,%;
///pressure:大气压力,Pa;
///temperature:大气温度,℃;
///humidity:相对湿度,%;
let airVol pressure temperature humidity =
    //MPa
    let satur_pressure = SaturWater.saturPressure(temperature)

    //水蒸汽百分比,%
    let vol_H2O = satur_pressure * 1e6 / pressure * humidity

    //干空气百分比,%
    let vol_dry = 100.0 - vol_H2O

    { Gas.zero with
        H2O = vol_H2O
        O2  = 0.21 * vol_dry
        N2  = 0.79 * vol_dry
    }

///wetAir:湿空气质量百分比,%;
///pressure:大气压力,Pa;
///temperature:大气温度,℃;
///humidity:相对湿度,%;
let wetAir pressure temperature humidity =
    let volumes = airVol pressure temperature humidity
    let wts = volumes * Gas.molar

    wts * Gas.unit(1e2 / wts.total())