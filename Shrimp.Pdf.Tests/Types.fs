[<AutoOpen>]
module Types
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open System.Threading
open iText.Kernel.Pdf
open Fake.IO.FileSystemOperators

open Shrimp.FSharp.Plus
open iText.Kernel.Geom
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Colors
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Parser
open iText.Kernel.Colors
open iText.Kernel.Pdf.Canvas.Parser.Data

[<RequireQualifiedAccess>]
type SeparationColor =
    | Black 
    | White
    | Separation of FsSeparation
with 
    member x.LoggingText =
        match x with 
        | SeparationColor.Black -> "Black"
        | SeparationColor.White -> "White"
        | Separation v -> v.LoggingText

    member private x.PredicateColor_ValueColorAndSeparation(color: AlternativeFsColor) =
        match x with 
        | SeparationColor.Black -> 
            match color with 
            | AlternativeFsColor.ValueColor valueColor ->
                match valueColor with 
                | FsValueColor.Cmyk cmyk -> cmyk.AsGray  |> Option.map AlternativeFsColor.valueColor 
                | FsValueColor.Gray v -> Some v |> Option.map AlternativeFsColor.valueColor 
                | FsValueColor.Rgb _ -> failwithf "Cannot separate colors by rgb"
                | FsValueColor.Lab _ -> failwithf "Cannot separate colors by lab"

            | _ -> None

        | SeparationColor.White -> failwithf "Not implemented"
        | SeparationColor.Separation separationColor ->
            match color.IsEqualsTo(
                AlternativeFsColor.Separation separationColor,
                ValueEqualOptions.DefaultValue_SeparationComparisonOptions_Name
            ) with 
            | true -> Some separationColor |> Option.map AlternativeFsColor.Separation
            | false -> None

    member x.PredicateColor(color: FsColor) =
        match color with 
        | FsColor.DeviceN (deviceN1, deviceN2) -> 
            match x with 
            | SeparationColor.Separation separation ->
                let findedDeviceNElement =
                    deviceN2.Elements.AsList
                    |> List.tryFind(fun m -> m.ColorName.ReadableName.EqualIC separation.Name)

                match findedDeviceNElement with 
                | None -> None
                | Some findedDeviceNElement ->
                    failwithf ""

            failwithf "Not implemented"
        | _ ->

            match color.AsAlternativeFsColor with 
            | Some color -> 
                match color with 
                | AlternativeFsColor.Separation separationColor ->
                    match separationColor.Name with 
                    | String.EndsWithIC "All" -> Some separationColor |> Option.map AlternativeFsColor.Separation
                    | _ -> x.PredicateColor_ValueColorAndSeparation(color)

                | AlternativeFsColor.IccBased iccBased ->
                     x.PredicateColor_ValueColorAndSeparation(AlternativeFsColor.ValueColor iccBased.Color)

                | _ -> x.PredicateColor_ValueColorAndSeparation(color)

            | None -> None 


            



[<AutoOpen>]
module _Separations =
    type Modifier with 
        static member RenderAsWhite_Shadable(): ShadableModifierIM<_> = 
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
                            |> PdfCanvas.concatMatrixByTransform ctm
                            |> PdfCanvas.setFillColor(DeviceGray.GRAY :> Color)
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
                            | FsColor.PatternColor _ 
                            | FsColor.ShadingColor _ -> 
                                failwithf "Not implemented"
                            | _ ->
                                let args =
                                    { CurrentRenderInfo = info 
                                      PageModifingArguments = args.PageModifingArguments }
                                Modifier.ReplaceColor(fun _ -> Some (DeviceGray.WHITE :> Color)) args

                        | IIntegratedRenderInfoIM.Pixel _ ->
                            failwithf "Not implemented"
            }


    type Modify with 
        static member CreateSeparationColors(separationColor: SeparationColor) =
            Modify.Create_Record_Shadarable(
                PageSelector.All,
                selectorAndModifiersList = 
                    [
                        { SelectorAndModifiersRecordShadableIM.Name = sprintf "Create Separation %s" separationColor.LoggingText
                          Selector = Selector.All(fun args info ->
                            match info with 
                            | :? IIntegratedRenderInfo as info ->
                               let b = 
                                   Info.ColorIs(FillOrStrokeOptions.FillOrStroke, fun color ->
                                        separationColor.PredicateColor(color).IsNone
                                   ) args info

                               b
                            | _ -> true
                          )
                          Modifiers = [
                            Modifier.RenderAsWhite_Shadable()
                          ]
                        }
                    ]

            )

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"

let cuttingLineSeparation =
    { FsSeparation.Name = "CuttingLine"
      BaseColor = FsValueColor.RGB_BLUE
      Transparency = 1.0 }

let cuttingLineSeparationZH =
    { FsSeparation.Name = "刀版"
      BaseColor = FsValueColor.RGB_BLUE
      Transparency = 1.0 }

let createTestPath file = 
    Path.changeExtension ".tests.pdf" file
    

let runTest file flow =
    let newPath = createTestPath file
    File.Copy(file, newPath, true)

    run newPath flow

let runManyWithBackup (files: string list) outputDir flow =
    Directory.ensure outputDir
    let newPaths =
        files 
        |> List.map (fun path ->
            let newPath = 
                outputDir </> Path.GetFileName(path)
            File.Copy(path, newPath, true)
            newPath
        )

    runMany Configuration.DefaultValue newPaths flow