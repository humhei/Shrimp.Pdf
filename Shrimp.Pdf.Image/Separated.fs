namespace Shrimp.Pdf
#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Image
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors
open iText.Kernel.Colors

open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Colorspace



[<RequireQualifiedAccess>]
type SeparaTargetColor =
    | DeviceN of PdfSpecialCs.DeviceN * FsDeviceN
    | White 
    | Gray of FsGray
    | Keep of FsSeparation

[<RequireQualifiedAccess>]
type SeparaColor =
    | Black 
    | White
    | Separation of FsSeparation
with 

    member x.LoggingText =
        match x with 
        | SeparaColor.Black -> "Black"
        | SeparaColor.White -> "White"
        | Separation v -> v.LoggingText

    member private x.PredicateColor_ValueColorAndSeparation(color: AlternativeFsColor) =
        match x with 
        | SeparaColor.Black -> 
            match color with 
            | AlternativeFsColor.Separation separationColor ->
                match separationColor.Name with 
                | String.EqualIC "Black" ->
                    x.PredicateColor(FsColor.ValueColor separationColor.Color)

                | _ -> SeparaTargetColor.White

            | AlternativeFsColor.ValueColor valueColor ->
                match valueColor with 
                | FsValueColor.Cmyk cmyk -> 
                    match cmyk.AsGray with 
                    | None -> SeparaTargetColor.White
                    | Some gray -> SeparaTargetColor.Gray gray
                | FsValueColor.Gray v -> v |> SeparaTargetColor.Gray
                | FsValueColor.Rgb _ -> failwithf "Cannot separate colors by rgb"
                | FsValueColor.Lab _ -> failwithf "Cannot separate colors by lab"

            | _ -> SeparaTargetColor.White

        | SeparaColor.White -> failwithf "Not implemented"
        | SeparaColor.Separation separationColor ->
            match color.IsEqualsTo(
                AlternativeFsColor.Separation separationColor,
                ValueEqualOptions.DefaultValue_SeparationComparisonOptions_Name
            ) with 
            | true -> SeparaTargetColor.Keep separationColor
            | false -> SeparaTargetColor.White

    member x.PredicateColor(color: FsColor) =
        match color with 
        | FsColor.DeviceN (deviceN1, deviceN2) -> 
            match x with 
            | SeparaColor.Black ->
                let findedDeviceNElement =
                    deviceN2.Elements.AsList
                    |> List.tryFind(fun m -> m.ColorName.ReadableName.EqualIC "Black")

                match findedDeviceNElement with 
                | None -> (SeparaTargetColor.White)
                | Some grayDeviceN ->
                    SeparaTargetColor.Gray(FsGray (float32 (1. - grayDeviceN.ColorValue)))


            | SeparaColor.Separation separation ->
                let findedDeviceNElement =
                    deviceN2.Elements.AsList
                    |> List.tryFind(fun m -> m.ColorName.ReadableName.EqualIC separation.Name)

                match findedDeviceNElement with 
                | None -> (SeparaTargetColor.White)
                | Some findedDeviceNElement ->
                    let deviceN2 = 
                        deviceN2.Elements.AsList
                        |> List.map(fun m -> 
                            match m.ColorName.ReadableName = findedDeviceNElement.ColorName.ReadableName with 
                            | true -> m
                            | false -> { m with ColorValue = 0.}
                        )
                        |> AtLeastOneList.Create
                        |> FsDeviceN

                    SeparaTargetColor.DeviceN(deviceN1, deviceN2)

            | _ -> SeparaTargetColor.White

        | _ ->

            match color.AsAlternativeFsColor with 
            | Some color -> 
                match color with 
                | AlternativeFsColor.Separation separationColor ->
                    match separationColor.Name with 
                    | String.EndsWithIC "All" -> SeparaTargetColor.Keep separationColor 
                    | _ -> x.PredicateColor_ValueColorAndSeparation(color)

                | AlternativeFsColor.IccBased iccBased ->
                     x.PredicateColor_ValueColorAndSeparation(AlternativeFsColor.ValueColor iccBased.Color)

                | _ -> x.PredicateColor_ValueColorAndSeparation(color)

            | None -> SeparaTargetColor.White


            



[<AutoOpen>]
module _Separations =
    type Modifier with 
        static member RenderAsWhite_Shadable(picker: FsColor -> SeparaTargetColor): ShadableModifierIM<_> = 
            let white = DeviceGray.WHITE :> Color
            {
                SHShadingModifier = Some (
                    fun args shadingInfo operatorRange pdfCanvas ->
                        let operatorRanges = shadingInfo.AccumulatedPathOperatorRanges
                        let shadingInfo = shadingInfo.PathRenderInfo :?> PdfShadingPathRenderInfo
                        let ctm = shadingInfo.ShadingColor.Ctm |> AffineTransformRecord.ofMatrix
                        let ctm = ctm.Inverse()
                        let fillingRule = shadingInfo.GetRule()
                        PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->

                            pdfCanvas
                            |> PdfCanvas.concatMatrixByTransformRecord ctm
                            |> PdfCanvas.setFillColor(white)
                            |> ignore

                            for operatorRange in operatorRanges do
                                PdfCanvas.writeOperatorRange operatorRange pdfCanvas |> ignore

                            pdfCanvas
                            |> PdfCanvas.closePathByOperation PathRenderInfo.FILL fillingRule
                        )
                )

                ClippingPathShadingModifier = None
                CommonModifierIM = 
                    fun args ->
                        match args.CurrentRenderInfoIM with 
                        | IIntegratedRenderInfoIM.Vector info ->
                            let color = (info.Value.GetFillColor() |> FsColor.OfItextColor)
                            match color with 
                            | FsColor.PatternColor _ -> failwithf "Not implemented"
                            | FsColor.ShadingColor _ -> 
                                failwithf "Invalid token"
                            | _ ->
                                let args =
                                    { CurrentRenderInfo = info 
                                      PageModifingArguments = args.PageModifingArguments }

                                Modifier.ReplaceColor(fun color ->
                                    match picker color with 
                                    | SeparaTargetColor.White -> Some white
                                    | SeparaTargetColor.Keep _ -> None
                                    | SeparaTargetColor.DeviceN (deviceN1, deviceN2) ->
                                        DeviceN(deviceN1, deviceN2.Values) :> Color
                                        |> Some
                                    | SeparaTargetColor.Gray gray ->
                                        gray.ToItextColor() :> Color
                                        |> Some
                                        
                                ) args

                        | IIntegratedRenderInfoIM.Pixel _ -> ModifierIM.RemoveImage() args
            }


    type Modify with 
        static member CreateSeparaColors(separationColor: SeparaColor) =
            Modify.Create_Record_Shadarable(
                PageSelector.All,
                selectorAndModifiersList = 
                    [
                        { SelectorAndModifiersRecordShadableIM.Name = sprintf "Create Separation %s" separationColor.LoggingText
                          Selector = Selector.All(fun args info -> true)
                          Modifiers = [
                            Modifier.RenderAsWhite_Shadable(separationColor.PredicateColor)
                          ]
                        }
                    ]

            )
