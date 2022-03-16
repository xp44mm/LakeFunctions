namespace LakeFunctions

type Liquid = 
    {
        ``CaSO4*2H2O``     : float
        ``CaSO3*(1/2)H2O`` : float
        CaCO3              : float
        MgSO4              : float
        MgCO3              : float
        inerts             : float
        ash                : float
        CaF2               : float
        MgF2               : float
        ``Cl-``            : float
        ``F-``             : float
        ``Mg++``           : float
        ``Ca++``           : float
        ``SO4--``          : float
        H2O                : float

    }

    member this.apply(func) =
        {
            ``CaSO4*2H2O``      = func this.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O``  = func this.``CaSO3*(1/2)H2O``
            CaCO3               = func this.CaCO3
            MgSO4               = func this.MgSO4
            MgCO3               = func this.MgCO3
            inerts              = func this.inerts
            ash                 = func this.ash
            CaF2                = func this.CaF2
            MgF2                = func this.MgF2
            ``Cl-``             = func this.``Cl-``
            ``F-``              = func this.``F-``
            ``Mg++``            = func this.``Mg++``
            ``Ca++``            = func this.``Ca++``
            ``SO4--``           = func this.``SO4--``
            H2O                 = func this.H2O
        }

    member this.apply(func) =
        {
            ``CaSO4*2H2O``     = func "CaSO4*2H2O" this.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = func "CaSO3*(1/2)H2O" this.``CaSO3*(1/2)H2O``
            CaCO3              = func "CaCO3" this.CaCO3
            MgSO4              = func "MgSO4" this.MgSO4
            MgCO3              = func "MgCO3" this.MgCO3
            inerts             = func "inerts" this.inerts
            ash                = func "ash" this.ash
            CaF2               = func "CaF2" this.CaF2
            MgF2               = func "MgF2" this.MgF2
            ``Cl-``            = func "Cl-" this.``Cl-``
            ``F-``             = func "F-" this.``F-``
            ``Mg++``           = func "Mg++" this.``Mg++``
            ``Ca++``           = func "Ca++" this.``Ca++``
            ``SO4--``          = func "SO4--" this.``SO4--``
            H2O                = func "H2O" this.H2O
        }

    member this.toSeq() =
        seq [
            "CaSO4*2H2O", this.``CaSO4*2H2O``
            "CaSO3*(1/2)H2O", this.``CaSO3*(1/2)H2O``
            "CaCO3", this.CaCO3
            "MgSO4", this.MgSO4
            "MgCO3", this.MgCO3
            "inerts", this.inerts
            "ash", this.ash
            "CaF2", this.CaF2
            "MgF2", this.MgF2
            "Cl-", this.``Cl-``
            "F-", this.``F-``
            "Mg++", this.``Mg++``
            "Ca++", this.``Ca++``
            "SO4--", this.``SO4--``
            "H2O", this.H2O
        ]
    
    member this.total() = this.toSeq() |> Seq.sumBy snd

    member this.reverse() = this.apply(fun v -> if v = 0.0 then 1.0 else 0.0)

    static member unit(l) =
        {
            ``CaSO4*2H2O``     = l
            ``CaSO3*(1/2)H2O`` = l
            CaCO3              = l
            MgSO4              = l
            MgCO3              = l
            inerts             = l
            ash                = l
            CaF2               = l
            MgF2               = l
            ``Cl-``            = l
            ``F-``             = l
            ``Mg++``           = l
            ``Ca++``           = l
            ``SO4--``          = l
            H2O                = l
        }

    static member zero = Liquid.unit(0.0)
    static member single = Liquid.unit(1.0)



    static member (*)(liq1: Liquid, liq2: Liquid) = 
        {
            ``CaSO4*2H2O``     = liq1.``CaSO4*2H2O``    * liq2.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = liq1.``CaSO3*(1/2)H2O``* liq2.``CaSO3*(1/2)H2O``
            CaCO3              = liq1.CaCO3             * liq2.CaCO3
            MgSO4              = liq1.MgSO4             * liq2.MgSO4
            MgCO3              = liq1.MgCO3             * liq2.MgCO3
            inerts             = liq1.inerts            * liq2.inerts
            ash                = liq1.ash               * liq2.ash
            CaF2               = liq1.CaF2              * liq2.CaF2
            MgF2               = liq1.MgF2              * liq2.MgF2
            ``Cl-``            = liq1.``Cl-``           * liq2.``Cl-``
            ``F-``             = liq1.``F-``            * liq2.``F-``
            ``Mg++``           = liq1.``Mg++``          * liq2.``Mg++``
            ``Ca++``           = liq1.``Ca++``          * liq2.``Ca++``
            ``SO4--``          = liq1.``SO4--``         * liq2.``SO4--``
            H2O                = liq1.H2O               * liq2.H2O
        
        }

    static member (/)(liq1: Liquid, liq2: Liquid) = 
        {
            ``CaSO4*2H2O``     = liq1.``CaSO4*2H2O``    / liq2.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = liq1.``CaSO3*(1/2)H2O``/ liq2.``CaSO3*(1/2)H2O``
            CaCO3              = liq1.CaCO3             / liq2.CaCO3
            MgSO4              = liq1.MgSO4             / liq2.MgSO4
            MgCO3              = liq1.MgCO3             / liq2.MgCO3
            inerts             = liq1.inerts            / liq2.inerts
            ash                = liq1.ash               / liq2.ash
            CaF2               = liq1.CaF2              / liq2.CaF2
            MgF2               = liq1.MgF2              / liq2.MgF2
            ``Cl-``            = liq1.``Cl-``           / liq2.``Cl-``
            ``F-``             = liq1.``F-``            / liq2.``F-``
            ``Mg++``           = liq1.``Mg++``          / liq2.``Mg++``
            ``Ca++``           = liq1.``Ca++``          / liq2.``Ca++``
            ``SO4--``          = liq1.``SO4--``         / liq2.``SO4--``
            H2O                = liq1.H2O               / liq2.H2O
        
        }

    static member (+)(liq1: Liquid, liq2: Liquid) = 
        {
            ``CaSO4*2H2O``     = liq1.``CaSO4*2H2O``    + liq2.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = liq1.``CaSO3*(1/2)H2O``+ liq2.``CaSO3*(1/2)H2O``
            CaCO3              = liq1.CaCO3             + liq2.CaCO3
            MgSO4              = liq1.MgSO4             + liq2.MgSO4
            MgCO3              = liq1.MgCO3             + liq2.MgCO3
            inerts             = liq1.inerts            + liq2.inerts
            ash                = liq1.ash               + liq2.ash
            CaF2               = liq1.CaF2              + liq2.CaF2
            MgF2               = liq1.MgF2              + liq2.MgF2
            ``Cl-``            = liq1.``Cl-``           + liq2.``Cl-``
            ``F-``             = liq1.``F-``            + liq2.``F-``
            ``Mg++``           = liq1.``Mg++``          + liq2.``Mg++``
            ``Ca++``           = liq1.``Ca++``          + liq2.``Ca++``
            ``SO4--``          = liq1.``SO4--``         + liq2.``SO4--``
            H2O                = liq1.H2O               + liq2.H2O
        
        }

    static member (-)(liq1: Liquid, liq2: Liquid) = 
        {
            ``CaSO4*2H2O``     = liq1.``CaSO4*2H2O``    - liq2.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = liq1.``CaSO3*(1/2)H2O``- liq2.``CaSO3*(1/2)H2O``
            CaCO3              = liq1.CaCO3             - liq2.CaCO3
            MgSO4              = liq1.MgSO4             - liq2.MgSO4
            MgCO3              = liq1.MgCO3             - liq2.MgCO3
            inerts             = liq1.inerts            - liq2.inerts
            ash                = liq1.ash               - liq2.ash
            CaF2               = liq1.CaF2              - liq2.CaF2
            MgF2               = liq1.MgF2              - liq2.MgF2
            ``Cl-``            = liq1.``Cl-``           - liq2.``Cl-``
            ``F-``             = liq1.``F-``            - liq2.``F-``
            ``Mg++``           = liq1.``Mg++``          - liq2.``Mg++``
            ``Ca++``           = liq1.``Ca++``          - liq2.``Ca++``
            ``SO4--``          = liq1.``SO4--``         - liq2.``SO4--``
            H2O                = liq1.H2O               - liq2.H2O
        
        }

    static member molar =
        {
            ``CaSO4*2H2O``     = MaterialProperties.molar "CaSO4*2H2O"
            ``CaSO3*(1/2)H2O`` = MaterialProperties.molar "CaSO3*(1/2)H2O"
            CaCO3              = MaterialProperties.molar "CaCO3"
            MgSO4              = MaterialProperties.molar "MgSO4"
            MgCO3              = MaterialProperties.molar "MgCO3"
            inerts             = 1.0
            ash                = 1.0
            CaF2               = MaterialProperties.molar "CaF2"
            MgF2               = MaterialProperties.molar "MgF2"
            ``Cl-``            = MaterialProperties.molar "Cl-"
            ``F-``             = MaterialProperties.molar "F-"
            ``Mg++``           = MaterialProperties.molar "Mg++"
            ``Ca++``           = MaterialProperties.molar "Ca++"
            ``SO4--``          = MaterialProperties.molar "SO4--"
            H2O                = MaterialProperties.molar "H2O"
        }

    static member suspend =
        { Liquid.zero with
            ``CaSO4*2H2O``     = 1.0
            ``CaSO3*(1/2)H2O`` = 1.0
            CaCO3              = 1.0
            MgSO4              = 1.0
            MgCO3              = 1.0
            inerts             = 1.0
            ash                = 1.0
            CaF2               = 1.0
            MgF2               = 1.0
        }

    static member suspendSG =
        { Liquid.zero with
            ``CaSO4*2H2O``     = MaterialProperties.specificGravity "CaSO4*2H2O"
            ``CaSO3*(1/2)H2O`` = MaterialProperties.specificGravity "CaSO3*(1/2)H2O"
            CaCO3              = MaterialProperties.specificGravity "CaCO3"
            MgSO4              = MaterialProperties.specificGravity "MgSO4"
            MgCO3              = MaterialProperties.specificGravity "MgCO3"
            inerts             = MaterialProperties.specificGravity "Inerts"
            ash                = MaterialProperties.specificGravity "Ash"
            CaF2               = MaterialProperties.specificGravity "CaF2"
            MgF2               = MaterialProperties.specificGravity "MgF2"
        }


    static member fw = Liquid.suspend.reverse()

    static member ion = { Liquid.fw with H2O = 0.0 }
