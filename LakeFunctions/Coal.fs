namespace LakeFunctions

///
type Coal = 
    {
        C: float
        H: float
        O: float
        N: float
        S: float
        H2O: float
        Ash: float
    }

    static member molar = 
        {
            C = MaterialProperties.molar "C"
            H = MaterialProperties.molar "H"
            O = MaterialProperties.molar "O"
            N = MaterialProperties.molar "N"
            S = MaterialProperties.molar "S"
            H2O = MaterialProperties.molar "H2O"
            Ash = 1.0
        }

    static member (*)(a: Coal, b: Coal) = 
        {
            C   = a.C   * b.C
            H   = a.H   * b.H
            O   = a.O   * b.O
            N   = a.N   * b.N
            S   = a.S   * b.S
            H2O = a.H2O * b.H2O
            Ash = a.Ash * b.Ash
        }

    static member (/)(a: Coal, b: Coal) = 
        {
            C   = a.C   / b.C
            H   = a.H   / b.H
            O   = a.O   / b.O
            N   = a.N   / b.N
            S   = a.S   / b.S
            H2O = a.H2O / b.H2O
            Ash = a.Ash / b.Ash
        }

    static member (+)(a: Coal, b: Coal) = 
        {
            C   = a.C   + b.C
            H   = a.H   + b.H
            O   = a.O   + b.O
            N   = a.N   + b.N
            S   = a.S   + b.S
            H2O = a.H2O + b.H2O
            Ash = a.Ash + b.Ash
        }

    static member (-)(a: Coal, b: Coal) = 
        {
            C   = a.C   - b.C
            H   = a.H   - b.H
            O   = a.O   - b.O
            N   = a.N   - b.N
            S   = a.S   - b.S
            H2O = a.H2O - b.H2O
            Ash = a.Ash - b.Ash
        }





