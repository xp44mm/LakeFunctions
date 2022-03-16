namespace LakeFunctions.Fluid

open MathNet.Numerics

module BendCorrect =

    ///圆角弯头角度修正系数.
    ///角度°,修正系数#
    let roundBendAngleCorrect =
        let xs,ys = 
            Lake.Fluid.圆角弯头角度修正系数.DataRecords
            |> Array.map(fun c -> c.angle,c.b)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    ///尖角弯头角度修正系数.
    ///角度°,修正系数#
    let sharpBendAngleCorrect =
        let xs,ys = 
            Lake.Fluid.尖角弯头角度修正系数.DataRecords
            |> Array.map(fun c -> c.angle,c.B)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    ///尖角弯头形状修正系数
    let sharpBendShapeCorrect =
        let xs,ys = 
            Lake.Fluid.尖角弯头形状修正系数.DataRecords
            |> Array.map(fun c -> c.a,c.C)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    ///小圆角弯头形状修正系数
    let smallRoundBendShapeCorrect =
        let xs,ys = 
            Lake.Fluid.小圆角弯头形状修正系数.DataRecords
            |> Array.map(fun c -> c.a,c.c)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    ///大圆角弯头形状修正系数
    let largeRoundBendShapeCorrect =
        let xs,ys = 
            Lake.Fluid.大圆角弯头形状修正系数.DataRecords
            |> Array.map(fun c -> c.a,c.c)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate