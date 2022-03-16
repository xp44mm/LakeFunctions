module LakeFunctions.Energy

///蒸发冷却过程，返回气体的饱和温度，℃，以及蒸发水量,kg；
///gas:气体成分，(分子式，质量(kg))；
///press: 绝对压力，Pa；
///temp:温度，℃；
let evaporativeCooling heat (gas: GasFlow) =
    //入口水量,kg
    let enWater = gas.H2O
    //当入口水量等于饱和水时，饱和温度等于入口温度,蒸发水量为0
    if enWater = GasFlowPerf.saturMoisture gas then (gas.temperature, 0.0)
    else
        //汽化潜热,kJ/kg
        let latentHeat = SaturWater.latentHeat(gas.pressure / 1e6)
        //入口空气的总焓, kJ
        let ein = GasFlowPerf.enthalpy gas + heat

        let rec bisect tmin tmax =
            let t = (tmin + tmax) / 2.0
            //当前循环温度下的饱和水量, kg
            let sw = GasFlowPerf.saturMoisture {gas with temperature = t}
            //当误差可以接受时，返回饱和温度℃,蒸发水量kg
            if tmax - tmin < 0.001 then (t, sw - enWater)
            else
                //出口气体的焓，不包括蒸发水焓, kJ
                let eout = GasFlowPerf.enthalpy {gas with temperature = t}
                //蒸发水热量, kJ
                let lheat = (sw - enWater) * latentHeat
                if eout + lheat > ein then bisect tmin t
                else bisect t tmax
        bisect 0.0 gas.temperature