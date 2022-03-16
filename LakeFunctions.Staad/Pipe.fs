namespace LakeFunctions.Sections

open LakeFunctions

type Pipe(dw, t) =
    let di = dw - 2.0 * t
    let i = System.Math.PI * (dw**4.0 - di**4.0) / 64.0

    member __.Dw = dw
    member __.T = t

    ///内径,mm
    member __.Di = di

    ///管子截面积,mm2
    member __.Area = System.Math.PI * (dw - t) * t

    ///管子惯性矩,mm4
    member __.IM = i
