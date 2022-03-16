namespace LakeFunctions.Sections

open LakeFunctions

///矩形
type Rectangle(height, width) = 
    let center = height / 20.
    let area = 0.01 * height * width
    let i = 0.0001 * height**3.0 * width / 12.
    let w = i / center
    
    ///矩形的高度，mm
    member __.Height = height
    
    ///矩形的宽度，mm
    member __.Width = width
    
    ///矩形的形心，cm
    member __.Center = center
    
    ///矩形的截面积，cm2
    member __.Area = area
    
    ///矩形的惯性矩，cm4
    member __.IM = i
    
    ///矩形的截面模量，cm3
    member __.W = w
