namespace LakeFunctions.Fluid.Ducts

module Spliters = 
    ///同心圆缓转弯头导流板的设置计算
    ///一片导流板
    let r1 r = 
        let cr = sqrt (r / (r + 1.0))
        r / cr
    
    ///二片导流板
    let s2 r = 
        let cr = (r / (r + 1.0)) ** (1. / 3.)
        let r1 = r / cr
        let r2 = r / cr / cr
        r1, r2
    
    ///三片导流板
    let s3 r = 
        let cr = (r / (r + 1.0)) ** (1. / 4.)
        let r1 = r / cr
        let r2 = r / cr / cr
        let r3 = r / cr / cr / cr
        r1, r2, r3
    
    let r2 r = 1.0 + r - exp (-r)
    
    let internal test (width, radius) = 
        let r = radius / width
        printfn "输入数据： w=%f,r=%f" width radius
        printfn "一片导流板： %A" (r1 r)
        printfn "二片导流板： %A" (s2 r)
        printfn "三片导流板： %A" (s3 r)
    
    test (100., 20.)
    System.Console.ReadLine() |> ignore

