namespace LakeFunctions

type GasFlow = 
    { 
        H2O : float
        O2 : float
        N2 : float
        CO2 : float
        SO2 : float
        SO3 : float
        HCl : float
        HF : float
        ash : float
        temperature : float
        pressure : float 
    }
    
    member this.Ingredient() : Gas = 
        { H2O = this.H2O
          O2 = this.O2
          N2 = this.N2
          CO2 = this.CO2
          SO2 = this.SO2
          SO3 = this.SO3
          HCl = this.HCl
          HF = this.HF
          ash = this.ash }
    
    static member create (gas : Gas, ?p, ?t) = 
        let t = defaultArg t 0.0
        let p = defaultArg p 0.0
        { H2O = gas.H2O
          O2 = gas.O2
          N2 = gas.N2
          CO2 = gas.CO2
          SO2 = gas.SO2
          SO3 = gas.SO3
          HCl = gas.HCl
          HF = gas.HF
          ash = gas.ash
          temperature = t
          pressure = p }
