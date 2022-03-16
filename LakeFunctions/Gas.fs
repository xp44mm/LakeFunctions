namespace LakeFunctions

type Gas =
    {
        H2O : float
        O2  : float
        N2  : float
        CO2 : float
        SO2 : float
        SO3 : float
        HCl : float
        HF  : float
        ash : float
    }

    member this.apply(func) =
        {
            H2O = func this.H2O
            O2  = func this.O2
            N2  = func this.N2
            CO2 = func this.CO2
            SO2 = func this.SO2
            SO3 = func this.SO3
            HCl = func this.HCl
            HF  = func this.HF
            ash = func this.ash
        }

    member this.apply(func) =
        {
            H2O  = func "H2O" this.H2O
            O2   = func "O2"  this.O2
            N2   = func "N2"  this.N2
            CO2  = func "CO2" this.CO2
            SO2  = func "SO2" this.SO2
            SO3  = func "SO3" this.SO3
            HCl  = func "HCl" this.HCl
            HF   = func "HF"  this.HF
            ash  = func "ash" this.ash
        }

    member this.toSeq() =
        seq [
            "H2O", this.H2O
            "O2", this.O2
            "N2", this.N2
            "CO2", this.CO2
            "SO2", this.SO2
            "SO3", this.SO3
            "HCl", this.HCl
            "HF", this.HF
            "ash", this.ash
        ]

    member this.total() = this.toSeq() |> Seq.sumBy snd

    member this.reverse() = this.apply(fun v -> if v = 0.0 then 1.0 else 0.0)
    
    static member unit(l) =
        {
            H2O  = l
            O2   = l
            N2   = l
            CO2  = l
            SO2  = l
            SO3  = l
            HCl  = l
            HF   = l
            ash  = l
        }
    
    static member zero = Gas.unit(0.0)


    static member (*)(gas1: Gas, gas2: Gas) = 
        {
            H2O  = gas1.H2O * gas2.H2O
            O2   = gas1.O2  * gas2.O2
            N2   = gas1.N2  * gas2.N2
            CO2  = gas1.CO2 * gas2.CO2
            SO2  = gas1.SO2 * gas2.SO2
            SO3  = gas1.SO3 * gas2.SO3
            HCl  = gas1.HCl * gas2.HCl
            HF   = gas1.HF  * gas2.HF
            ash  = gas1.ash * gas2.ash
        }

    static member (/)(gas1: Gas, gas2: Gas) =
        {
            H2O  = gas1.H2O / gas2.H2O
            O2   = gas1.O2  / gas2.O2
            N2   = gas1.N2  / gas2.N2
            CO2  = gas1.CO2 / gas2.CO2
            SO2  = gas1.SO2 / gas2.SO2
            SO3  = gas1.SO3 / gas2.SO3
            HCl  = gas1.HCl / gas2.HCl
            HF   = gas1.HF  / gas2.HF
            ash  = gas1.ash / gas2.ash
        }

    static member (+)(gas1: Gas, gas2: Gas) = 
        {
            H2O  = gas1.H2O + gas2.H2O
            O2   = gas1.O2  + gas2.O2
            N2   = gas1.N2  + gas2.N2
            CO2  = gas1.CO2 + gas2.CO2
            SO2  = gas1.SO2 + gas2.SO2
            SO3  = gas1.SO3 + gas2.SO3
            HCl  = gas1.HCl + gas2.HCl
            HF   = gas1.HF  + gas2.HF
            ash  = gas1.ash + gas2.ash
        }

    static member (-)(gas1: Gas, gas2: Gas) = 
        {
            H2O  = gas1.H2O - gas2.H2O
            O2   = gas1.O2  - gas2.O2
            N2   = gas1.N2  - gas2.N2
            CO2  = gas1.CO2 - gas2.CO2
            SO2  = gas1.SO2 - gas2.SO2
            SO3  = gas1.SO3 - gas2.SO3
            HCl  = gas1.HCl - gas2.HCl
            HF   = gas1.HF  - gas2.HF
            ash  = gas1.ash - gas2.ash
        }

    static member molar =
        {
            H2O  = MaterialProperties.molar "H2O"
            O2   = MaterialProperties.molar "O2"
            N2   = MaterialProperties.molar "N2"
            CO2  = MaterialProperties.molar "CO2"
            SO2  = MaterialProperties.molar "SO2"
            SO3  = MaterialProperties.molar "SO3"
            HCl  = MaterialProperties.molar "HCl"
            HF   = MaterialProperties.molar "HF"
            ash  = 1.0
        }

    static member harmful = 
        { Gas.zero with
            SO2 = 1.0
            SO3 = 1.0
            HCl = 1.0
            HF  = 1.0
            ash = 1.0 }

    static member dryGas =
        { Gas.unit(1.0) with
            H2O  = 0.0
            ash  = 0.0
        }



