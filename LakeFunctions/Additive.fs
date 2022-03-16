module LakeFunctions.Additive//此模块无依赖项

///acidType:酸类型；
///ppm：酸浓度；
///sgSln：吸收塔浆液的SG sln
let meq (acidType, ppm, sgSln) =
    let value, num =
        let dibasicValue =
            [|
                "Adipic", 146.0, 0.20
                "Succinic", 118.0, 0.23
                "Glutaric", 132.0, 0.56
                "Nitric", 63.0, 0.01 |]
            |> Array.sumBy (fun (_, v, p) -> p / v)
            |> (fun v -> 1.0 / v)
        [|
            "Adiptic", 146.0, 2
            "Formic", 46.03, 1
            "Dibasic", dibasicValue, 2
            "None", 100.0, 0 |]
        |> Array.find (fun (t, _, _) -> t = acidType)
        |> fun (_, v, n) -> v, n
    (float num) / value * ppm * sgSln

let rtu_quench (quench) =
    if quench then 1.0
    else 0.95

///acid meq
let rtu_acid (meq) =
    if meq = 0.0 then 1.0
    else (1.0079868 + 0.13277618 * meq) ** 0.5

let validate_acidMeq(noAcid, meq) =
    if noAcid then
        "OK"
    else
        if (meq < 2.0 || 14.0 < meq)
        then    "Consult Design - Acid meq Out of Range [2.0,14.0]"
        else    "OK"