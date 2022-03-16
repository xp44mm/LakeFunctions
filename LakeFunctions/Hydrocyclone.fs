module LakeFunctions.Hydrocyclone

let splitInput = 
    {Liquid.suspend with ash = 0.0
                         CaF2 = 0.0
                         MgF2 = 0.0}
let splitV = { 
    Liquid.zero with 
        ash = 1.0
        CaF2 = 1.0
        MgF2 = 1.0
    }

let splitOutput = splitInput.reverse()


///根据旋流站的输入，求fw值
///fwsplit,切分率,0~1;
///solids,0~1;
///split,0~1;
let fwsplit solids (split: Liquid) (feed: Liquid) = 
    //v,var,变化成分
    let feedv = splitV * feed
    let ffw = LiquidPerf.fw feed

    //c,const,固定不变成分
    let uctss =
        let ufc = splitInput * feed * split
        LiquidPerf.tss ufc
    
    //代入法求fw
    let rec loop fwsplit = 
        let ufv = feedv * Liquid.unit fwsplit
        //底流总悬浮固体
        let utss = uctss + LiquidPerf.tss ufv
        //底流溶液
        let ufw = utss * (1.0 - solids) / solids
        let fwsplit' = ufw / ffw

        if abs(fwsplit' - fwsplit) < 1e-9 
        then fwsplit'
        else loop fwsplit'
        
    loop split.ash
