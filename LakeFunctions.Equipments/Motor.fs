///电机
module LakeFunctions.Motor

//powern, 额定功率,kW
//volt, 电压,V
let volt powern =
    if powern < 200. then
        380.
    else
        6000.

//axis, 轴功率, kW
//powern, 额定功率,kW
let powern axis=
    Lake.Equipments.电机.DataRecords
    |> Array.find(fun m -> m.额定功率 / m.安全余量 >= axis)
    |> fun m -> m.额定功率

//margin,安全余量
let powernExact (margin:float) axis =
    Lake.Equipments.电机.DataRecords
    |> Array.find(fun p -> p.额定功率 / margin >= axis)
    |> fun m -> m.额定功率
