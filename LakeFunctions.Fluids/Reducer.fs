namespace LakeFunctions.Fluid

module Reducer =           

    let 缩小管阻力系数 d1 d2 a =
        let b = pown (d2 / d1) 2
        if a <= 45. then
            0.8 * sin(a * System.Math.PI / 360.) * (1. - b) ** 0.75
        elif a < 180. then
            0.5 * (sin(a * System.Math.PI / 360.)) ** 0.5 * (1. - b) ** 0.75
        else
            0.5 * (1. - b) ** 0.75

    let 扩大管阻力系数 d1 d2 a =
        let b = (d1 / d2) ** 2.0
        if a <= 45. then
            2.6 * sin(a * System.Math.PI / 360.) * (1. - b) ** 2.0
        else
            (1. - b) ** 2.0
 
    let 变径管阻力系数(sinlet:Shape,soutlet:Shape, angle: float) =
        //获取计算所用的截面尺寸值
        let size sect =
            match sect with
            |Rect(a,b) -> a
            |Circle(d) -> d

        let ddi = size sinlet
        let ddo = size soutlet
        if ddi > ddo then
            缩小管阻力系数 ddi ddo angle
        else
            扩大管阻力系数 ddi ddo angle

