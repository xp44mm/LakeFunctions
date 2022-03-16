module LakeFunctions.Liquids

///回流水去制浆系统
///solids,%
let toprep (limestone: Liquid) (reclaim: Liquid) solids = 
    let lsolids = LiquidPerf.solids limestone
    let ltf = LiquidPerf.tf limestone

    let rsolids = LiquidPerf.solids reclaim
    let rtf = LiquidPerf.tf reclaim
    let tf = ltf * (lsolids - solids) / (solids - rsolids)

    let scale = tf / rtf
    let toprep = reclaim * Liquid.unit scale
    toprep

///
type dewateringInput() = 
    member val ext = "" with get, set
    member val phof = "" with get, set
    member val bleed = "" with get, set
    member val shuf = "" with get, set
    member val solids = 0.0 with get, set
    member val concCl = 0.0 with get, set

type hydrocycloneInput() = 
    member val ``CaSO4*2H2O`` = 0.0 with get, set
    member val ``CaSO3*(1/2)H2O`` = 0.0 with get, set
    member val CaCO3 = 0.0 with get, set
    member val MgSO4 = 0.0 with get, set
    member val MgCO3 = 0.0 with get, set
    member val inerts = 0.0 with get, set
    member val solids = 0.0 with get, set
    
    member this.split = 
        {Liquid.zero with ``CaSO4*2H2O`` = this.``CaSO4*2H2O``
                          ``CaSO3*(1/2)H2O`` = this.``CaSO3*(1/2)H2O``
                          CaCO3 = this.CaCO3
                          MgSO4 = this.MgSO4
                          MgCO3 = this.MgCO3
                          inerts = this.inerts}
    //fw：自由水
    member this.getSplit(fw) = 
        {this.split with ash = fw
                         CaF2 = fw
                         MgF2 = fw
                         ``Cl-`` = fw
                         ``F-`` = fw
                         ``Mg++`` = fw
                         ``Ca++`` = fw
                         ``SO4--`` = fw
                         H2O = fw}

type vacuumFilterInput() = 
    member val ``CaSO4*2H2O`` = 0.0 with get, set
    member val ``CaSO3*(1/2)H2O`` = 0.0 with get, set
    member val CaCO3 = 0.0 with get, set
    member val MgSO4 = 0.0 with get, set
    member val MgCO3 = 0.0 with get, set
    member val inerts = 0.0 with get, set
    member val ash = 0.0 with get, set
    member val CaF2 = 0.0 with get, set
    member val MgF2 = 0.0 with get, set
    member val solids = 0.0 with get, set
    member val concCl = 0.0 with get, set

    member this.split = 
        {Liquid.zero with ``CaSO4*2H2O`` = this.``CaSO4*2H2O``
                          ``CaSO3*(1/2)H2O`` = this.``CaSO3*(1/2)H2O``
                          CaCO3 = this.CaCO3
                          MgSO4 = this.MgSO4
                          MgCO3 = this.MgCO3
                          inerts = this.inerts
                          ash = this.ash
                          CaF2 = this.CaF2
                          MgF2 = this.MgF2}

///专用于有废水站的情况
type shofBleedLoopData = 
    {t: float
     fwx: float
     fwz: float}

type shofBleedFinishingResult =
    {
        gypsumBleed: Liquid
        phof: Liquid
        phuf: Liquid

        vffeed: Liquid
        gypsum: Liquid
        wash: float
        filtrate: Liquid

        shff: Liquid
        shof: Liquid
        shuf: Liquid

        reclaimWater: Liquid
    }

///计算出浆液的fw，返回浆液
///依据比例计算出固体与离子含量，然后根据Cl-离子浓度计算需要添加多少水
let fwLiquid (productA: Liquid) susp solids concCl = 
    let tss = LiquidPerf.tss susp
    let fw = tss / solids * (1.0 - solids)
    let cl = concCl / 1e6 * fw
    let scale = cl / productA.``Cl-``
    let ion = Liquid.unit(scale) * productA
    let tds = LiquidPerf.tds ion
    {(Liquid.suspend * susp + Liquid.ion * ion) with H2O = fw - tds}


///废水站排废水
let shofChlorideBleed (productA: Liquid) (opt: dewateringInput) (ph: hydrocycloneInput) (vf: vacuumFilterInput) 
    (sh: hydrocycloneInput) = 
    let fwLiquid = fwLiquid productA
    let variables = 
        let case = Dewatering.case opt.ext opt.phof opt.bleed opt.shuf
        let case = if case = 0 then 1 else case // 容错
        Dewatering.variables case
    
    let y = vf.split
    
    let finishing (res:shofBleedLoopData) = 
        let x = ph.getSplit res.fwx//一级旋流站,液体成分变量
        let z = sh.getSplit res.fwz//废水旋流站,液体成分变量
        let split = Dewatering.split variables res.t x y z
        let stream = Dewatering.stream split
        
        // gypsumBleed,吸收塔排出浆液
        let gypsumBleed = 
            let susp = productA / stream.product()
            fwLiquid susp opt.solids opt.concCl

        let vffeed = gypsumBleed * stream.``7``()

        let gypsum = 
            let susp = gypsumBleed * stream.``8``()
            fwLiquid susp vf.solids vf.concCl

        ///石膏冲洗水
        let wash = VacuumFilter.wash gypsum opt.concCl
                    
        let filtrate = 
            let f = vffeed - gypsum
            {f with H2O = f.H2O + wash}
        
        ///
        let reclaimed = gypsumBleed * stream.``11``() // filtrate + gypsumBleed * (stream.``28``() + stream.``23.2``())

        {
            gypsumBleed = gypsumBleed
            phof = gypsumBleed * stream.``5``()
            phuf = gypsumBleed * stream.``6``()

            vffeed = vffeed
            gypsum = gypsum
            wash = wash
            filtrate = filtrate

            shff = gypsumBleed * stream.``25``()
            shof = gypsumBleed * stream.``26.1``()
            shuf = gypsumBleed * stream.``27``()

            reclaimWater = reclaimed
        }

    let rec loop(res:shofBleedLoopData) = 
        let x = ph.getSplit res.fwx//一级旋流站切分率
        let z = sh.getSplit res.fwz//废水旋流站切分率
        //LiquidCalc!A88
        let split = Dewatering.split variables res.t x y z
        let stream = Dewatering.stream split
        
        //吸收塔浆液排出
        let gypsumBleed = 
            let susp = productA / stream.product()
            fwLiquid susp opt.solids opt.concCl

        //一级旋流站
        let fwx' = Hydrocyclone.fwsplit ph.solids x gypsumBleed
        if abs(fwx' - res.fwx) > 1e-12 then loop {res with fwx = fwx'}
        else
            //废水旋流站
            let phof = gypsumBleed * stream.``5``()
            let fwz' = Hydrocyclone.fwsplit sh.solids z phof

            if abs(fwz' - res.fwz) > 1e-12 then 
                loop {res with fwx = fwx'
                               fwz = fwz'}
            else
                //计算石膏产量
                let gypsum = 
                    let susp = gypsumBleed * stream.``8``()
                    fwLiquid susp vf.solids vf.concCl
                
                //废水需要排放的Cl-量
                let cl = productA.``Cl-`` - gypsum.``Cl-``

                //sh feed cl：反算废水站入口cl-量
                let cl25 = cl / (1.0 - fwz')
                let t' = 1.0 - cl25 / phof.``Cl-``
                if abs(t' - res.t) > 1e-12 then 
                    loop {fwx = fwx'
                          fwz = fwz'
                          t = t'}
                else {
                        fwx = fwx'
                        fwz = fwz'
                        t = t'}
    
    loop {fwx = 0.0
          fwz = 0.0
          t = 0.0}
    |> finishing

