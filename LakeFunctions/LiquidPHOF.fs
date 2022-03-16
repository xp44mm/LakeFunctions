module LakeFunctions.LiquidPHOF

open Liquids

type phofBleedLoopData =
    {
        bleedSclale: float //= bleed / phof
        fwx: float
    }

type phofBleedFinishingResult =
    {
        gypsumBleed: Liquid
        phof: Liquid
        phuf: Liquid

        vffeed: Liquid
        gypsum: Liquid
        wash: float
        filtrate: Liquid

        bleed: Liquid //Chloride Bleed

        reclaimWater: Liquid // s11

    }

///一级旋流站顶流排废水
let phofChlorideBleed (productA: Liquid) (opt: dewateringInput) (ph: hydrocycloneInput) (vf: vacuumFilterInput) =

    let fwLiquid = fwLiquid productA

    let variables =
        let case = Dewatering.case opt.ext opt.phof opt.bleed opt.shuf
        let case = if case = 0 then 1 else case // 容错
        Dewatering.variables case

    let y = vf.split

    let finishing (res: phofBleedLoopData) =
        let x = ph.getSplit res.fwx//一级旋流站,液体成分变量
        let split = Dewatering.split variables res.bleedSclale x y Liquid.zero
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

        let phof = gypsumBleed * stream.``5``()
        let bleed = phof * Liquid.unit res.bleedSclale

        let reclaimed = gypsumBleed * stream.``11``()

        {
            gypsumBleed = gypsumBleed
            phof = phof
            phuf = gypsumBleed * stream.``6``()

            vffeed = vffeed
            gypsum = gypsum
            wash = wash
            filtrate = filtrate

            bleed = bleed

            reclaimWater = reclaimed
        }

    let rec loop(res: phofBleedLoopData) =
        let x = ph.getSplit res.fwx//一级旋流站切分率
        let z = Liquid.zero
        //LiquidCalc!A88
        let split = Dewatering.split variables res.bleedSclale x y z
        let stream = Dewatering.stream split

        //吸收塔浆液排出
        let gypsumBleed =
            let susp = productA / stream.product()
            fwLiquid susp opt.solids opt.concCl

        //一级旋流站
        let fwx' = Hydrocyclone.fwsplit ph.solids x gypsumBleed
        if abs(fwx' - res.fwx) > 1e-12 then loop {res with fwx = fwx'}
        else
                //计算石膏产量
                let gypsum =
                    let susp = gypsumBleed * stream.``8``()
                    fwLiquid susp vf.solids vf.concCl

                //废水需要排放的Cl-量
                let cl = productA.``Cl-`` - gypsum.``Cl-``

                let phof = gypsumBleed * stream.``5``()

                let scale = cl / phof.``Cl-``

                if abs(scale - res.bleedSclale) > 1e-12 then
                    loop {fwx = fwx'
                          bleedSclale = scale}
                else {
                        fwx = fwx'
                        bleedSclale = scale}

    loop {fwx = 0.0
          bleedSclale = 0.0}
    |> finishing