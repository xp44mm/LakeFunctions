namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions


type DewateringTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.caseSplit() = 
        let data =
            [
                6,"p",1
            ]
        for r,c,expected in data do
            let actual = Dewatering.caseSplit r c
            Assert.Equal(expected, actual)
    

    [<Fact>]
    member this.case() = 
        let opt = new Liquids.dewateringInput()
        opt.solids <- 0.15
        opt.shuf <- "to Absorber"
        opt.phof <- "to Filtrate Tank"
        opt.ext <- "Primary + Secondary"
        opt.concCl <- 2e4
        opt.bleed <- "SH OF"

        let case = Dewatering.case opt.ext opt.phof opt.bleed opt.shuf
        Assert.Equal(13, case)

        let variables = 
            let case = if case = 0 then 1 else case // 容错
            Dewatering.variables case

        let expected = {
                        p = 1;
                        q = 1;
                        r = 1;
                        s = 0;
                        t = -1;
                        u = 1;
                        v = 1;
                        w = 0;
                        x = -1;
                        y = -1;
                        z = -1;}:Dewatering.dewateringVariables<int>
        
        Assert.Equal(expected, variables)
        