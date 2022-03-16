namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type StaadTest(output: ITestOutputHelper) = 
    [<Fact>]
    member this.buckling() = 
        let data =
            [
                50.,0.888
                52.5,0.877
                169.27, 0.24489799999999998
            ]

        for lam,expected in data do
            let actual = Staad.buckling lam
            output.WriteLine(sprintf "%A" actual)
            Assert.Equal(expected, actual)

    [<Fact>]
    member this.elasticModulus()=
        let x = Steel.elasticModulus "Q235" 126.0
        //output.WriteLine(sprintf "%A" x)
        Assert.Equal(197.92, x)

    [<Fact>]
    member this.allowableStress()=
        let x = Steel.allowableStress "Q235" 102.0
        //output.WriteLine(sprintf "%A" x)
        Assert.Equal(125.0, x)


