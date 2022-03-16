namespace LakeFunctions.Sections

open System.Text.RegularExpressions
open LakeFunctions

type ShapeSteelSection =
    {Spec: string //说明
     Height: float //mm
     Width: float //mm
     Center: float //cm
     Area: float //cm2
     IMx: float //cm4
                }

open LakeFunctions.Sections.ShapeSteel

type CombSection(spec: string, t: float) =

    //型钢截面
    let rib =
        match spec with
        | C obj ->
            {Spec = obj.Spec
             Height = obj.H
             Width = obj.B
             Center = obj.H / 20.
             Area = obj.Area
             IMx = obj.IMx}
        | I obj ->
            {Spec = obj.Spec
             Height = obj.H
             Width = obj.B
             Center = obj.H / 20.
             Area = obj.Area
             IMx = obj.IMx}
        | H obj ->
            {Spec = obj.Spec
             Height = obj.H
             Width = obj.B
             Center = obj.H / 20.
             Area = obj.Area
             IMx = obj.IMx}
        | EA obj ->
            {Spec = obj.Spec
             Height = obj.B
             Width = obj.D
             Center = obj.B / 10. - obj.Z0
             Area = obj.Area
             IMx = obj.IMx}
        | UA obj ->
            {Spec = obj.Spec
             Height = obj.B1
             Width = obj.D
             Center = obj.B1 / 10. - obj.Y0
             Area = obj.Area
             IMx = obj.IMx}
        | FB(w, t) ->
            let rect = new Rectangle(w, t)
            {Spec = sprintf "FB%f×%f" w t
             Height = rect.Height
             Width = rect.Width
             Center = rect.Center
             Area = rect.Area
             IMx = rect.IM}

    let plate = new Rectangle(t, 30. * t + rib.Width)
    let _area = plate.Area + rib.Area
    let _weight = 0.785 * _area //，kg/m
    //cm
    let ribCenter = rib.Center + plate.Height / 10.
    //cm
    let _center = (rib.Area * ribCenter + plate.Area * plate.Center) / _area
    let _ix =
        rib.IMx + rib.Area * (_center - ribCenter)**2.0 + plate.IM
        + plate.Area * (_center - plate.Center)**2.0
    //cm
    let totalHeight = rib.Height / 10. + plate.Height / 10.
    let _zmin = _ix / max (_center) (totalHeight - _center)

    member __.thick = t
    member __.spec = spec

    ///组合截面的截面积，cm2
    member __.area = _area

    ///组合截面的重量，kg/m
    member __.weight = _weight

    ///组合截面形心到内表面的距离，cm
    member __.center = _center

    ///组合截面的惯性矩，cm4
    member __.ix = _ix

    ///组合截面的截面模量，cm3
    member __.zmin = _zmin

    ///，kg/m
    member __.ribWeight = 0.785 * rib.Area
