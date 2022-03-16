namespace LakeFunctions.Fluid

type Shape =
    | Rect of width:float*height:float
    | Circle of float

    member this.Area = 
        match this with
        |Rect(a,b) -> a*b
        |Circle(d) -> System.Math.PI/4.*d**2.0
