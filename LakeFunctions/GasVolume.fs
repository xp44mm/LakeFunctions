module LakeFunctions.GasVolume

type gasVolume =
    {
        //d-dry,n-standard
        nvolume : float
        volume  : float

        dnvolume: float
        dvolume : float
    }

let nvol (gas:Gas) =
    let moles = gas / Gas.molar
    moles.total() * 22.414

let calc (gas:GasFlow) =
    let correct = IdealGas.volumeCorrect gas.pressure gas.temperature
    let cleanGas = { (gas.Ingredient()) with ash = 0.0 }
    let dryGas = { cleanGas with H2O = 0.0 }

    let nvolume = nvol cleanGas
    let volume = nvolume * correct
    let dnvolume = nvol dryGas
    let dvolume = dnvolume * correct

    {
        nvolume = nvolume
        volume = volume
        dnvolume = dnvolume
        dvolume = dvolume
    }




















