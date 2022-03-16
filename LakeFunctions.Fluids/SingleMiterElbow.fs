namespace LakeFunctions.Fluid

type SingleMiterElbow( diameter //截面直径，m
                     , angle   //转角,°
                     ) =

    let area = System.Math.PI * diameter ** 2.0 / 4.0

    let zeta = 1.3 * BendCorrect.sharpBendAngleCorrect(angle)

    ///阻力系数
    member __.Zeta = zeta
    ///截面积,m
    member __.Area = area
