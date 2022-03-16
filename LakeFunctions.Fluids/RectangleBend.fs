namespace LakeFunctions.Fluid


type RectangleBend( widthInlet  //
                  , widthOutlet //
                  , height      //
                  , radius      //
                  , angle       //
                  ) =
    let 矩形弯头Zeta(b1: float, b2: float, h: float, r: float, a: float) =
            if b1 = b2 then
                if r = 0. then
                    //等截面尖角弯头
                    1.4 * BendCorrect.sharpBendShapeCorrect(h / b1) * BendCorrect.sharpBendAngleCorrect(a)
                else
                    //等截面急转弯头
                    Bend.急转弯头基本阻力系数(r / b1) * BendCorrect.smallRoundBendShapeCorrect(h / b1) * BendCorrect.roundBendAngleCorrect(a)
            else
                if r = 0. then
                    //变截面尖角弯头
                    Bend.变截面尖角弯头基本阻力系数(b2 / b1) * BendCorrect.sharpBendShapeCorrect(h / b1) * BendCorrect.sharpBendAngleCorrect(a)
                else
                    //变截面急转弯头
                    Bend.变截面急转弯头基本阻力系数(r / min b1 b2)(b2 / b1) * BendCorrect.smallRoundBendShapeCorrect(h / b1) * BendCorrect.roundBendAngleCorrect(a)


    let area = (min widthInlet widthOutlet) * height
    let zeta = 矩形弯头Zeta (widthInlet, widthOutlet, height, radius, angle)

    ///阻力系数
    member __.Zeta = zeta
    ///截面积,m
    member __.Area = area
