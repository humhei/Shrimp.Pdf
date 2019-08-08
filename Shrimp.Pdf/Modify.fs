namespace Atrous.Pdf

open Parser.Core.Editor

[<RequireQualifiedAccess>]
type Modify =
    | Retain
    | Remove
    | Fix of (FixArgument -> unit)

[<RequireQualifiedAccess>]
module Modify =
    open iText.Kernel.Pdf.Canvas
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open iText.Kernel.Colors
    open Atrous.Pdf.Extensions

    let private modify f =
        Modify.Fix <| fun arg ->
            let info = arg.Info
            arg.Canvas.FixState arg.Operands (f info.Value)

    let rec toFunction (md: Modify) =
        let remove = 
            modify (fun info (gs: CanvasGraphicsState) -> 
                AbstractRenderInfo.cata 
                    (fun _ -> [PdfCanvas.setTextRendingMode(TextRenderingMode.INVISIBLE)]) 
                    (fun _ -> [PdfCanvas.newPath])
                    info
            )

        match md with 
        | Modify.Fix fix -> fix
        | Modify.Remove -> toFunction remove
        | Modify.Retain -> toFunction remove

    let dash (dashArray: float list) =
        modify (fun info (gs: CanvasGraphicsState) -> 
            let dashArray = dashArray |> List.map float32 |> Array.ofList
            let _,phase = CanvasGraphicsState.getDashPattern gs
            let phase = float32 phase
            [
                PdfCanvas.setDashpattern dashArray phase
            ]
        )

    let strokeColor color =
        modify (fun info (gs: CanvasGraphicsState) -> 
            [
                PdfCanvas.setStrokeColor color
            ]
        )

    let remove =
        Modify.Remove

    let retain = Modify.Retain

    let fillColor color = 
        modify (fun info (gs: CanvasGraphicsState) -> 
            [
                PdfCanvas.setFillColor color
            ]
        )

    let text str =
        modify (fun info (gs: CanvasGraphicsState) -> 
            [
                PdfCanvas.showText str
            ]
        )
    type private ApperanceSetting = AbstractRenderInfo -> list<PdfCanvas -> PdfCanvas>

    module private ApperanceSetting =
        open Atrous

        let create (setFill: (PdfCanvas -> PdfCanvas)) (setStroke: (PdfCanvas -> PdfCanvas)) : ApperanceSetting =
            fun info ->
                match info with 
                | :? PathRenderInfo as info ->
                    match info.GetOperation() with 
                    | PathRenderInfo.FILL ->
                        [
                            setFill
                        ]
                    | PathRenderInfo.STROKE ->
                        [
                            setStroke
                        ]
                    | PathRenderInfo.FILLANDSTROKE ->
                        [
                            setFill
                            setStroke
                        ]
                    | others -> 
                        Logger.notSupportedPathRendingMode others
                        []

                | :? TextRenderInfo as info ->
                        match info.GetTextRenderMode() with 
                        | TextRenderingMode.FILL_STROKE ->
                            [
                                setFill
                                setStroke
                            ]
                        | TextRenderingMode.STROKE ->
                            [
                                setStroke
                            ]
                        | TextRenderingMode.FILL ->
                            [
                                setFill
                            ]
                        | others -> 
                            Logger.notSupportedTextRendingMode others
                            []
                | _ -> failwith "Not supported"

    let colorWith (map: Color -> Color) =
        modify (fun info (gs: CanvasGraphicsState) -> 
            let mappedFillColor = map (gs.GetFillColor())
            let setFill = PdfCanvas.setFillColor mappedFillColor
            let mappedStrokeColor = map (gs.GetStrokeColor())
            let setStroke = PdfCanvas.setStrokeColor mappedStrokeColor
            ApperanceSetting.create setFill setStroke info
        )

    let color c = colorWith (fun _ -> c)
        
        


