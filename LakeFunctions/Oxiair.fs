module LakeFunctions.Oxiair

///根据氧气质量，得到空气质量
let feed (air : GasFlow) o2 =
    let ingr = (air.Ingredient()) * Gas.unit (o2 / air.O2)
    GasFlow.create (ingr, p = air.pressure, t = air.temperature)

///风机温升公式
///T1 = fan inlet temperature,℃
///T2 = fan outlet temperature,℃
///p1 = fan inlet pressure,absolute
///p2 = fan outlet pressure,absolute
let fanOutletTemperature(t1,p1,p2) =
    let k1 = t1 + 273.15
    let k2 = k1*(p2/p1)**0.286
    k2 - 273.15

let compress (feed: GasFlow) p =
    { feed with
        temperature = fanOutletTemperature(feed.temperature,feed.pressure,p)
        pressure = p
    }

let satur (compressed: GasFlow) =
    let temp, evapor = Energy.evaporativeCooling 0.0 compressed

    { compressed with
        H2O = compressed.H2O + evapor
        temperature = temp
    }