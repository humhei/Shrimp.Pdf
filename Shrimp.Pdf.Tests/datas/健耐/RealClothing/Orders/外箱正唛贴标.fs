namespace RealClothing.Orders
module 外箱正唛细节 =
    open Atrous.Entities.Types

    type CartonDetail =
        {
            Color: ColorEnum
            CartonArt: string
            CartonBarcode: string
        }

    let cartonDetailMap =
        [
            "18SPN35",
                [
                    { Color = ColorEnum.Black          ; CartonArt = "771756"; CartonBarcode = "16001001222201"}
                    { Color = ColorEnum.Burgundy       ; CartonArt = "771757"; CartonBarcode = "16001001222263"}
                    { Color = ColorEnum.``Dusty blue`` ; CartonArt = "771758"; CartonBarcode = "16001001222324"}
                    { Color = ColorEnum.Navy           ; CartonArt = "771759"; CartonBarcode = "16001001222386"}
                    { Color = ColorEnum.Pink           ; CartonArt = "771760"; CartonBarcode = "16001001222430"}
                ]
        ] |> Map