namespace LakeFunctions.Ribs

open System.Text.RegularExpressions
open LakeFunctions.Sections

module Stable =

    //槽钢稳定性公式,p187,t6.4-1
    let channelStable h b t len xigma = 
        570.0 * b * t / len / h * 125.0 / xigma

    //工字钢稳定性公式,p189,t6.4-2
    let ishapeStable h b t len area iry wx xigma = 
        let lambda = (len / 10.0) / iry
        let e = len * t / b / h
        //仅用于均布荷载作用于上翼缘
        let beta =
            if e >=2.0 then
                0.95
            else
                0.69 + 0.13 * e
        beta * 4320.0 / lambda**2.0 * area * (0.1 * h) / wx * sqrt (1.0 + (lambda * t / 4.4 / h)**2.0) * 125.0 / xigma

    let ribStable (spec:string) len xigma =

        let stbl =
            match spec.[0] with
            | '[' -> 
                let channel = ShapeSteel.Channels.[spec]
                channelStable channel.H channel.B channel.T len xigma

            | 'I' -> 
                let ishape = ShapeSteel.Ishapes.[spec]
                ishapeStable ishape.H ishape.B ishape.T len ishape.Area ishape.IRy ishape.Wx xigma

            | 'H' ->
                let hs = ShapeSteel.Hshapes.[spec]
                ishapeStable hs.H hs.B hs.T1 len hs.Area hs.IRy hs.Wx xigma
            
            | 'L' ->
                if (Regex.Matches(spec, @"\d+").Count = 2 ) then
                    let ea = ShapeSteel.Eangles.[spec]
                    channelStable ea.B ea.B ea.D len xigma
                else
                    let ua = ShapeSteel.Uangles.[spec]
                    channelStable ua.B1 ua.B2 ua.D len xigma

            | 'F' -> 0.1

            | _ -> 0.0

        if stbl > 0.6 then
            1.1 - 0.4646 / stbl + 0.1269 / stbl ** 1.5
        else
            stbl
