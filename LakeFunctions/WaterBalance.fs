module LakeFunctions.WaterBalance
open Cuisl

let s4 = 2.0 * MaterialProperties.molar "H2O" / MaterialProperties.molar "CaSO4*2H2O"
let s3 = 0.5 * MaterialProperties.molar "H2O" / MaterialProperties.molar "CaSO3*(1/2)H2O"

let liqwater(liquid: Liquid) = 
    [
        liquid.H2O
        s4 * liquid.``CaSO4*2H2O``
        s3 * liquid.``CaSO3*(1/2)H2O``
    ]|> Seq.sum

type makeupInput =
    {
        gasInlet :float 
        oxifeed  :float 
        sootblow :float 
        limestone:float 
        removal  :float
        gasOutlet:float 
        gypsum   :Liquid
        cl       :Liquid
    }
///补给水
let makeup (m:makeupInput) =
    let gasInlet  = m.gasInlet
    let oxifeed   = m.oxifeed
    let sootblow  = m.sootblow
    let limestone = m.limestone
    let removal   = m.removal
    let gasOutlet = m.gasOutlet
    let gypsum    = m.gypsum
    let cl        = m.cl

    [
        - gasInlet
        - oxifeed
        - sootblow
        - limestone
        removal //是负数
        gasOutlet
        liqwater gypsum
        liqwater cl
    ]|> Seq.sum

///除雾器冲洗水,kg/hr;
///dia,吸收塔直径,m;
let mistEliminator dia =
    let v0 = 0.27 // (m3/hr)/m2
    let area = System.Math.PI / 4. * dia * dia
    let v = v0 * area
    v * 1e3