namespace LakeFunctions.Sections

open System.Text.RegularExpressions

module ShapeSteel = 

    let Channels = 
        Lake.ShapeSteel.槽钢.DataRecords
        |> Array.map(fun o -> o.Spec,o)
        |> Map.ofArray

        //use db = new LakeContext()
        //db.槽钢
        //|> Seq.map(fun o -> o.Spec,o)
        //|> dict
    
    let Ishapes = 
        Lake.ShapeSteel.工字钢.DataRecords
        |> Array.map(fun o -> o.Spec,o)
        |> Map.ofArray

        //use db = new LakeContext()
        //db.工字钢
        //|> Seq.map(fun o -> o.Spec,o)
        //|> dict

    
    let Hshapes = 
        Lake.ShapeSteel.H型钢.DataRecords
        |> Array.map(fun o -> o.Spec,o)
        |> Map.ofArray


        //use db = new LakeContext()
        //db.H型钢
        //|> Seq.map(fun o -> o.Spec,o)
        //|> dict
    
    let Eangles = 
        Lake.ShapeSteel.等边角钢.DataRecords
        |> Array.map(fun o -> o.Spec,o)
        |> Map.ofArray

        //use db = new LakeContext()
        //db.等边角钢
        //|> Seq.map(fun o -> o.Spec,o)
        //|> dict

    
    let Uangles = 
        Lake.ShapeSteel.不等边角钢.DataRecords
        |> Array.map(fun o -> o.Spec,o)
        |> Map.ofArray

        //use db = new LakeContext()
        //db.不等边角钢
        //|> Seq.map(fun o -> o.Spec,o)
        //|> dict


    let extractNumbers inp = 
        Regex.Matches(inp,@"\d+(\.\d+)?")
        |> Seq.cast<Match>
        |> Seq.map(fun m -> float m.Value)
        |> Seq.toArray

    let (|C|I|H|EA|UA|FB|)(spec: string) = 
        match spec.[0] with
        | '[' -> C Channels.[spec]
        | 'I' -> I Ishapes.[spec]
        | 'H' -> H Hshapes.[spec]
        | 'L' -> 
            match extractNumbers spec with
            | [|_; _|] -> EA Eangles.[spec]
            | [|_; _; _|] -> UA Uangles.[spec]
            | _ -> failwith ""
        | 'F' -> 
            match extractNumbers spec with
            | [|w; t|] -> FB(w, t)
            | _ -> failwith ""
        | _ -> failwith spec
    
    ///型钢的重量kg/m
    let weight = 
        function 
        | C obj -> obj.Weight
        | I obj -> obj.Weight
        | H obj -> obj.Weight
        | EA obj -> obj.Weight
        | UA obj -> obj.Weight
        | FB(w, t) -> 0.785 * w * t / 100.0
    
    ///截面积，cm2
    let area = 
        function 
        | C obj -> obj.Area
        | I obj -> obj.Area
        | H obj -> obj.Area
        | EA obj -> obj.Area
        | UA obj -> obj.Area
        | FB(w, t) -> w * t / 100.0
    
    ///强轴惯性矩，cm4
    let IMx = 
        function 
        | C obj -> obj.IMx
        | I obj -> obj.IMx
        | H obj -> obj.IMx
        | EA obj -> obj.IMx
        | UA obj -> obj.IMx
        | FB(w, t) -> (Rectangle(w, t)).IM
    
    ///强轴截面模量，cm3
    let Wx = 
        function 
        | C obj -> obj.Wx
        | I obj -> obj.Wx
        | H obj -> obj.Wx
        | EA obj -> obj.Wx
        | UA obj -> obj.Wx
        | FB(w, t) -> (Rectangle(w, t)).W
    
    ///弱轴惯性矩，cm4
    let IMy = 
        function 
        | C obj -> obj.IMy
        | I obj -> obj.IMy
        | H obj -> obj.IMy
        | EA obj -> obj.IMx
        | UA obj -> obj.IMy
        | FB(w, t) -> (Rectangle(t, w)).IM
    
    ///弱轴截面模量，cm3
    let Wy = 
        function 
        | C obj -> obj.Wy
        | I obj -> obj.Wy
        | H obj -> obj.Wy
        | EA obj -> obj.Wx
        | UA obj -> obj.Wy
        | FB(w, t) -> (Rectangle(t, w)).W


    //errata:需要查阅型钢手册,cm
    let Z0 =
        function 
        | C obj -> obj.Z0
        | I obj -> 0.0
        | H obj -> 0.0
        | EA obj -> obj.Z0
        | UA obj -> failwith ""
        | FB(w, t) -> 0.0

    //errata:需要查阅型钢手册,mm
    let B =
        function 
        | C obj -> obj.B
        | I obj -> obj.B
        | H obj -> obj.B
        | EA obj -> obj.B
        | UA obj -> obj.B2
        | FB(w, t) -> t
