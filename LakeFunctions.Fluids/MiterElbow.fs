namespace LakeFunctions.Fluid

open MathNet.Numerics
open System

module MiterElbow =

    ///虾米弯焊缝条数
    let joint r =
        if   r  = 0.5 then 1
        elif r <= 1.3 then 2
        elif r <= 1.9 then 3
        elif r <= 4.2 then 4
        else 6

    ///虾米弯阻力系数
    let zeta = 
        //query {
        //    for c in db.虾米弯阻力系数 do
        //    select (c.R,c.Kz) //x,y
        //} |> Cuisl.MathNet.interpolate
        let xs,ys = 
            Lake.Fluid.虾米弯阻力系数.DataRecords
            |> Array.map(fun c -> c.r,c.kz)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    type MiterElbow( diameter //截面直径，m
                   , radius  //转弯半径，m
                   , angle   //转角,°
                   ) =
        let r = radius / diameter
        let zeta = zeta(r) * BendCorrect.roundBendAngleCorrect(angle)
        let area = Math.PI * pown diameter 2 / 4.0
        let weld = joint(r)

        ///阻力系数
        member __.Zeta = zeta

        ///截面积,m
        member __.Area = area

        ///焊缝根数,根
        member __.Weld = weld