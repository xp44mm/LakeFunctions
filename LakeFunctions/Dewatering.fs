module LakeFunctions.Dewatering

//open LakeEf

let dewateringCase (ext, ph, bleed, sh) = 
    match (ext, ph, bleed, sh) with
    | 1, _, _, _ -> 1
    | 2, 1, 1, _ -> 2
    | 2, 1, 2, _ -> 3
    //| 2, 1, _, _ -> 3
    | 2, 2, 1, _ -> 4
    | 2, 2, 2, _ -> 5
    //| 2, 2, _, _ -> 5
    | 3, 1, 1, _ -> 6
    | 3, 1, 2, _ -> 7
    | 3, 1, 3, 1 -> 8
    | 3, 1, 3, 2 -> 9
    | 3, 1, 4, _ -> 10
    | 3, 2, 1, _ -> 11
    | 3, 2, 2, _ -> 12
    | 3, 2, 3, 1 -> 13
    | 3, 2, 3, 2 -> 14
    | 3, 2, 4, _ -> 15
    | _ -> 0

///脱水系统配置工况
let case ext ph bleed sh = 
    let indexofExt = 
        function 
        | "None" -> 1
        | "Primary Only" -> 2
        | "Primary + Secondary" -> 3
        | _ -> 0

    let indexofPH = 
        function 
        | "to Absorber" -> 1
        | "to Filtrate Tank" -> 2
        | _ -> 0

    let indexofBleed = 
        function 
        | "None" -> 1
        | "PH OF" -> 2
        | "SH OF" -> 3
        | "Filtrate" -> 4
        | _ -> 0

    let indexofSH = 
        function 
        | "to Absorber" -> 1
        | "to Filter Feed Tank" -> 2
        | _ -> 0


    let ext   = ext   |> indexofExt
    let ph    = ph    |> indexofPH
    let bleed = bleed |> indexofBleed
    let sh    = sh    |> indexofSH
    dewateringCase (ext, ph, bleed, sh)

///分量的数据类型
type dewateringVariables<'t> = 
    { p : 't
      q : 't
      r : 't
      s : 't
      t : 't //一级旋流站顶流分到二级旋流站的比例
      u : 't
      v : 't //皮带机滤液直排废水的比例
      w : 't //一级旋流站顶流直排废水的比例
      x : 't
      y : 't
      z : 't }

let private caseSplitTbl =
    Lake.balance.caseSplits.DataRecords
    |> Array.map(fun c -> c.index,c(*c.P,c.Q,c.R,c.S,c.T,c.U,c.V,c.W,c.X,c.Y,c.Z*))
    |> Map.ofArray

    //use db = new LakeContext()
    //query {
    //    for c in db.caseSplits do
    //        select (c.Index,c(*c.P,c.Q,c.R,c.S,c.T,c.U,c.V,c.W,c.X,c.Y,c.Z*))
    //        }
    ////|> Array.ofSeq
    //|> Map.ofSeq


///根据配置取得每例的整体变量值
///返回三个值：0无，1全，-1根据输入确定切分率
let variables case = 
    let vals = caseSplitTbl.[case]

        //|> Array.map(fun cols ->
        //    cols |> Map.map(fun _ v -> 0(*toint v*))
        //)


    //let caseSplit = 
    //    fun row col -> vals.[row - 1].[col]

    { p = vals.p(*caseSplit case "p"*)
      q = vals.q(*caseSplit case "q"*)
      r = vals.r(*caseSplit case "r"*)
      s = vals.s(*caseSplit case "s"*)
      t = vals.t(*caseSplit case "t"*)
      u = vals.u(*caseSplit case "u"*)
      v = vals.v(*caseSplit case "v"*)
      w = vals.w(*caseSplit case "w"*)
      x = vals.x(*caseSplit case "x"*)
      y = vals.y(*caseSplit case "y"*)
      z = vals.z(*caseSplit case "z"*) }


///液体成分分项变量值
///t: PH OF reclaim
///v: VF to Filtrate
///w: PH OF to Filtrate
///tvw: t or v or w 之一，只用到其中一个值
///x: PH split
///y: VF split
///z: SH split
///0~1
let split (variables: dewateringVariables<int>) tvw x y z = 
    let p0 = variables.p
    let q0 = variables.q
    let r0 = variables.r
    let s0 = variables.s
    let t0 = variables.t
    let u0 = variables.u
    let v0 = variables.v
    let w0 = variables.w
    let x0 = variables.x
    let y0 = variables.y
    let z0 = variables.z
    
    let suspendc p0 = 
        match p0 with
        | 0 -> Liquid.unit 0.0
        | 1 -> Liquid.unit 1.0
        | _ -> failwith ""
    
    let suspendv t0 tvw = 
        match t0 with
        | -1 -> tvw
        | _ -> suspendc t0
    
    {p = suspendc p0
     q = suspendc q0
     r = suspendc r0
     s = suspendc s0
     t = suspendv t0 <| Liquid.unit tvw
     u = suspendc u0
     v = suspendv v0 <| Liquid.unit tvw
     w = suspendv w0 <| Liquid.unit tvw
     x = suspendv x0 x
     y = suspendv y0 y
     z = suspendv z0 z}



/// 0~1，以吸收塔浆液为基准，求每支浆液的成分
type stream (split : dewateringVariables<Liquid>) = 
    let p = split.p
    let q = split.q
    let r = split.r
    let s = split.s
    let t = split.t
    let u = split.u
    let v = split.v
    let w = split.w
    let x = split.x
    let y = split.y
    let z = split.z

    /// complement;
    /// 0~1
    let comp liq = Liquid.unit 1.0 - liq

    ///gypsumBleed
    member this.``4``() = Liquid.unit 1.0
    member this.``5``() = this.``4``() * comp x
    member this.``5.1``() = this.``5``()  * comp p
    member this.``26.4``() = this.``5.1``() * comp w
    member this.``28``() = this.``5.1``() * w
    member this.``5.2``() = this.``5``() * p * q
    member this.``23``() = this.``5.2``() * t
    member this.``23.1``() = this.``23``() * comp r
    member this.``23.2``() = this.``23``() * r
    member this.``24``() = this.``5.2``() * comp t
    member this.``26.3``() = this.``24``() * comp u
    member this.``25``() = this.``24``() * u

    ///shof
    member this.``26.1``() = this.``25``() * comp z
    member this.``27``() = this.``25``() * z
    member this.``27.1``() = this.``27``() * comp s
    member this.``27.2``() = this.``27``() * s
    member this.``5.3``() = this.``5``() * p * comp q
    member this.``6``() = this.``4``() * x
    member this.``7``() = this.``6``() + this.``27.2``()

    ///gypsum
    member this.``8``() = this.``7``() * y

    member this.``9``() = this.``7``() - this.``8``()
    member this.``26.2``() = this.``9``() * comp v
    member this.``10``() = this.``9``() * v

    /// reclaim water
    member this.``11``() = this.``10``() + this.``23.2``() + this.``28``()

    // total reclaim water
    member this.reclaim() = this.``11``() + this.``23.1``() + this.``27.1``()


    //productA
    member this.product() = comp <| this.reclaim()


//type dewateringOption() = 
//    member val ext = "" with get, set
//    member val ph = "" with get, set
//    member val bleed = "" with get, set
//    member val sh = "" with get, set
//    member this.case = case this.ext this.ph this.bleed this.sh
//    member this.variables = 
//        let case = if this.case = 0 then 1 else this.case // 容错
//        variables case


//*********ExcelFunctions.BalanceFunctions********************
///返回三个值：0无，1全，-1根据输入确定切分率
let caseSplit casing streamName = 
    let split = caseSplitTbl.[casing]
    match streamName with
    | "p" -> split.p
    | "q" -> split.q
    | "r" -> split.r
    | "s" -> split.s
    | "t" -> split.t
    | "u" -> split.u
    | "v" -> split.v
    | "w" -> split.w
    | "x" -> split.x
    | "y" -> split.y
    | "z" -> split.z
    | _ -> failwith streamName

/// 26.1,26.2,26.3,26.4,or none
let caseBleed =
    let lookup =
        [
            1 ,0
            2 ,0
            3 ,3
            4 ,0
            5 ,4
            6 ,0
            7 ,3
            8 ,1
            9 ,1
            10,2
            11,0
            12,4
            13,1
            14,1
            15,2
        ]|> Map.ofList
    fun cs -> lookup.[cs]

let caseWash cs =
    if cs <= 5 then 0
    else caseBleed cs

//浆液中悬浮固体在脱水系统的返回率，0~1
let recycleRatio p q r s t u v w x y z =
    let b = (1.0 - x) * (1.0 - p) * w
    let c = (1.0 - x) * p * (1.0 - q)
    let d = (1.0 - x) * p * q * t * (1.0 - r)
    let e = (1.0 - x) * p * q * t * r
    let f = (1.0 - x) * p * q * (1.0 - t) * u * z * (1.0 - s)
    let g = (1.0 - x) * p * q * (1.0 - t) * u * z * s
    let h = x * (1.0 - y) * v
    let i = g * (1.0 - y) * v
    b + c + d + e + f + g + h + i

///悬浮浆液相对于水的比重
let suspendedSpecificGravity (components:(string*float)list) =
    let components =
        components
        |> List.filter (fun (_,wt) -> wt > 0.0)

    if components.IsEmpty then 1.0 else

    let totalWeight =
        components
        |> List.sumBy(fun(_,wt)-> wt)

    let numerator =
        components
        |> List.sumBy(fun(form,wt)->
            let sg = MaterialProperties.specificGravity form
            sg * wt
        )

    numerator / totalWeight
