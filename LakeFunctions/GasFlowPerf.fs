module LakeFunctions.GasFlowPerf

///除去气体中的灰成分
let deash (gas:Gas) = { gas with ash = 0.0 }

///中和酸所需碱的量, kmol/hr
///removal,kg/hr
let alkalinity stoich (removal : Gas) =
    let factor =
        { Gas.zero with
            SO2 = stoich
            SO3 = 1.0
            HCl = 0.5
            HF = 0.5
        }
    let alk = factor * removal / Gas.molar
    alk.total()

///返回给定成分的气体在给定温度下的总焓(kJ).
///成分的每一项对应成分的分子式，和成分的质量(kg).
///温度单位是℃。
///与气体的压力无关
let enthalpy (gas:GasFlow) =
    let ingr = deash (gas.Ingredient())
    ingr.apply(IdealGas.enthalpy gas.temperature).total()

///饱和气体的绝对湿度,kg；
///气体成分，(分子式，质量(kg))；
///温度℃；
///绝对压力，Pa；
let saturMoisture (gas:GasFlow) =
    //饱和湿度,0~1
    let humidity = SaturWater.saturPressure gas.temperature / (gas.pressure * 1e-6)

    //干气体的摩尔数(kmol)
    let dryGas = {(gas.Ingredient()) with H2O = 0.0} / Gas.molar
    let saturWater = dryGas.total() * humidity / (1.0 - humidity)
    saturWater * MaterialProperties.molar "H2O"

let nvolume(gas:Gas) = (deash gas / Gas.molar).total() * 22.414

let totalVolume (gas:GasFlow) =
    let normalVolume = (deash (gas.Ingredient()) / Gas.molar).total() * 22.414
    normalVolume * IdealGas.volumeCorrect gas.pressure gas.temperature