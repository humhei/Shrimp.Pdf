namespace Atrous.Pdf

module Manipulates =
    open FParsec
    open Atrous.Pdf.PageReader
    open iText.Kernel.Colors
    open iText.Kernel.Pdf
    open iText.Kernel.Pdf.Canvas
    open Fake.IO
    open Types
    open Atrous.Pdf.Parser
    open Atrous.Pdf.Colors.ColorConverter
    open iText.Layout
    open iText.Kernel
    open Atrous.Pdf.Colors
    open iText.Layout.Properties
    open Atrous.Pdf.Extensions
    open Atrous.Pdf.Operators
    open Atrous.Utils
    open Atrous.Pdf.Filters
    open iText.Kernel.Geom
    open Atrous.Pdf.Parser.Operators
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Atrous.Pdf.Select
    open Atrous.Pdf.DocReader

    let private fixWithSelect select (modify: Modify) (arg: ManipulateArgument<_>) =
        let f = modify |> Modify.toFunction

        let select = 
            let origin = arg.Select <&> select
            match modify with 
            | Modify.Fix _ | Modify.Remove -> origin
            | Modify.Retain -> origin >> not

        let infos = fix arg.Page select f
        { arg.FlowState with 
            LastManipulatesInfos = infos }

    let fixWithChoice (pss: list<AbstractRenderInfo -> bool>) modify =
        let filter = choice pss
        fixWithSelect filter modify

    let fixPathWithChoice (pss: list<list<PathRenderInfo -> bool>>) modify =
        let filter = pss |> List.map path |> choice
        fixWithSelect filter modify

    let fixPathWith (ps: list<PathRenderInfo -> bool>) modify =
        fixWithSelect (path ps) modify

    [<AutoOpen>]
    module Fix = 
        let changeDashPatternOfPath (dashArray: float list) = 
            fixWithSelect (path [Path.dash]) (Modify.dash dashArray)
        
        let replaceText (pattern: string) (replacement: string) =
            fixWithSelect (text [Text.prediate ((=) pattern)]) (Modify.text replacement)

        let replaceStrokeColorOfPath (origin: Color list) (replacement: Color) =
            fixWithSelect (Select.path <&> Select.strokeColors origin) (Modify.strokeColor replacement)

        let replaceFillColorOfPath (origin: Color list) (replacement: Color) =
            fixWithSelect (Select.path <&> Select.fillColors origin) (Modify.fillColor replacement)

        let replaceColor (origin: Color list) (replacement: Color) =
            fixWithSelect (Select.colors origin) (Modify.color replacement)

        let blackOrWhite arg =

            let modify =
                Modify.colorWith
                    (fun color ->
                        if color = DeviceRgb.BLUE then color
                        else
                            match color with 
                            | :? DeviceRgb ->
                                let l = toLight color
                                if l < 99. then DeviceGray.BLACK :> Color
                                else DeviceGray.WHITE :> Color
                            | :? DeviceCmyk as cmyk ->
                                cmyk.GetColorValue() 
                                |> Seq.exists (fun v -> v > 0.01f)
                                |> function 
                                    | true -> DeviceGray.BLACK :> Color
                                    | false -> DeviceGray.WHITE :> Color

                            | :? DeviceGray ->
                                let l = toLight color
                                if l < 99. then DeviceGray.BLACK :> Color
                                else DeviceGray.WHITE :> Color

                            | _ -> 
                                printfn "Warning: blackOrWhite - can not parse color %A" color 
                                color
                    ) 
            fixWithSelect (Select.all) modify arg

    let private pathWithStrokeColors colors =
        path [Path.stroke colors]

    [<AutoOpen>]
    module Page = 

        let trimToStrokeColors (colors: Color list) (kind: PageBoxKind) arg =
        
            let filter = pathWithStrokeColors colors
            ManipulateArgument.readPaths [filter] arg
            |> Seq.collect PathRenderInfo.toPoints
            |> Rectangle.createFromPoints
            |> fun rect -> 
                PdfPage.setPageBox rect kind arg.Page |> ignore
            arg.FlowState

        let trimToStrokeColor (color: Color) (kind: PageBoxKind) arg =
            trimToStrokeColors [color] kind arg

        let trimToStrokeBlue arg =
            trimToStrokeColor DeviceRgb.BLUE PageBoxKind.AllBox arg
    
        let bleedOut margin arg =
            let rect = arg.Page.GetTrimBox() |> Rectangle.applyMargin margin
            arg.Page.SetCropBox(rect).SetMediaBox(rect) |> ignore
            arg.FlowState

        let trimToStrokeCyanOrMegenta arg =
            trimToStrokeColors [DeviceCmyk.CYAN; DeviceCmyk.MAGENTA] PageBoxKind.AllBox arg

        let applyMargin (margin: Margin) =
            fun arg ->
                let page = arg.Page
                let bbox = page.GetBBox()
                let rect = bbox |> Rectangle.applyMargin margin
                page |> PdfPage.setPageBox rect PageBoxKind.AllBox |> ignore
                arg.FlowState

        let rotatePage degree =
            fun arg ->
                arg.Page.SetRotation(degree) |> ignore
                arg.FlowState

        let clipping (margin: Margin) =  
            fun (arg:ManipulateArgument<_>) ->
                let page =arg.Page
                let box = page.GetBBox() |> Rectangle.applyMargin margin
                let command = sprintf "%f %f %f %f re W" <| box.GetXF() <| box.GetYF() <| box.GetWidthF() <| box.GetHeightF()
                page |> runLiteralCommand command
                arg.FlowState

        let clippingToTrimBox =  
            fun arg ->
                let page = arg.Page
                let box = PdfPage.getTrimBox page
                let command = sprintf "%f %f %f %f re W" <| box.GetXF() <| box.GetYF() <| box.GetWidthF() <| box.GetHeightF()
                page |> runLiteralCommand command
                arg.FlowState

    [<AutoOpen>]
    module RemoveAndRetain = 
        let removePathWith (ps: list<PathRenderInfo -> bool>) =
            fixPathWith ps Modify.remove

        let retainPathWith (ps: list<PathRenderInfo -> bool>) =
            fixPathWith ps Modify.retain

        let removeDashPath arg =
            arg 
            |> removePathWith [Path.dash]

        let removePathWithStrokeColors (colors: Color list) =
            fixWithSelect (pathWithStrokeColors colors) Modify.remove

        let removeText (filter: string -> bool) =
            fixWithSelect (text [Text.filter filter]) Modify.remove

        let removeTextWithFillColors (colors: Color list) =
            fixWithSelect (text [Text.fill colors]) Modify.remove

        let removeTextOfFParsec (parser: Parser<_,unit>) =
            let filter s =
                run parser s
                |> function
                    | ParserResult.Success _ -> true
                    | ParserResult.Failure _ -> false
            removeText filter

        let retainPathWithStrokeColors (colors: Color list) =
            fixWithSelect (pathWithStrokeColors colors) Modify.retain



    [<AutoOpen>]
    module Add = 
        let addBorderWith (bboxes: Rectangle seq) color arg =
            let page = arg.Page
            let canvas = new PdfCanvas(page)
            PdfCanvas.useCanvas canvas (fun _ ->
                canvas.SetStrokeColor(color) |> ignore
                bboxes |> Seq.iter(fun bbox ->
                    let height = bbox.GetHeightF()
                    canvas
                        .SetLineWidth(height / 50.)
                        .Rectangle(bbox)
                        .Stroke()
                        |> ignore
                )
            )

            arg.FlowState

        let addBorder color arg =
            let page = arg.Page
            let bbox = page.GetBBox()
            addBorderWith [bbox] color arg

        let addBorderToText color arg =

            let _,pageNum,parser = arg.Page,arg.PageNum,arg.Parser
            let rects = Extract.texts pageNum Select.visible parser |> Seq.map TextRenderInfo.getBound
            addBorderWith rects color arg

        let addExceedTextToTopLeft textArgs arg =
            ManipulateArgument.useFont textArgs.FontGen arg (fun font ->
                let page = arg.Page
                let bbox = page.GetBBox()
                let height = bbox.GetHeightF()
                Canvas.useCanvas page bbox (fun canvas ->
                    Canvas.addTextToRectWithCustomAlign 
                        TextAlignment.LEFT 
                        VerticalAlignment.BOTTOM 
                        (Orientation.LeftBottom (0.,0. + height))
                        textArgs.Text
                        font
                        textArgs.FontSize
                        textArgs.FontColor
                        canvas
                )
                arg.FlowState
            )

        let addColoredText (pos:Position) (texts: ColoredText list) fontGen fontSize =
            fun arg ->
                ManipulateArgument.useFont fontGen arg (fun font ->
                    let page = arg.Page
                    let doc = page.GetDocument() :?> PdfDocumentWithResources
                    let texts = texts |> List.map (ColoredText.toTextElement doc)
                    let bbox = Position.getPageBox page pos
                    Canvas.useCanvas page bbox (fun canvas ->
                        Canvas.addColoredTextToRectWithCustomAlian TextAlignment.LEFT VerticalAlignment.BOTTOM pos.Orientation texts font fontSize canvas |> ignore
                    )
                    arg.FlowState
                )


        let addTextWithCustomAlign textAlignment verticalAlignment (pos:Position) textArgs =
            fun arg ->
                ManipulateArgument.useFont textArgs.FontGen arg (fun font ->
                    let page = arg.Page
                    let bbox = Position.getPageBox page pos
                    Canvas.useCanvas page bbox (fun canvas ->
                        Canvas.addTextToRectWithCustomAlign textAlignment verticalAlignment pos.Orientation textArgs.Text font textArgs.FontSize textArgs.FontColor canvas |> ignore
                    )

                    arg.FlowState
                )

        let addText (pos:Position) textArgs =
            let textAlignment,verticalAlignment = Position.getPdfAlignment pos None
            addTextWithCustomAlign textAlignment verticalAlignment pos textArgs

        let addTextWithRotation (pos:Position) rotation textArgs =
            fun arg ->
                ManipulateArgument.useFont textArgs.FontGen arg (fun font ->
                    let page = arg.Page
                    let bbox = Position.getPageBox page pos
                    Canvas.useCanvas page bbox (fun canvas ->
                        Canvas.addTextToRectWithRotation pos.Orientation textArgs.Text font textArgs.FontSize textArgs.FontColor rotation canvas |> ignore
                    )
                    arg.FlowState
                )

        let addTextToRectWithScale text fontGen fontColor scale (bbox: Rectangle) =
            fun arg ->
                ManipulateArgument.useFont fontGen arg (fun font ->
                
                    let page = arg.Page
                    let lineHeightUnit = PdfFont.heightLerance
                    let lineWidthUnit = PdfFont.calculateLineWidthUnit text font
                    let fontSize =
                        let horizonalMaxSize = bbox.GetWidthF() / lineWidthUnit
                        let verticalMaxSize = bbox.GetHeightF() / lineHeightUnit
                        (min verticalMaxSize horizonalMaxSize) * scale

                    Canvas.useCanvas page bbox (fun canvas -> 
                        Canvas.addTextToRect (Orientation.Center) text font fontSize fontColor canvas |> ignore
                    )
                    arg.FlowState
                )


        let addTextToPageWithScale (kind:PageBoxKind) scale text fontGen fontColor =
            fun arg ->
                let page = arg.Page
                let bbox = PdfPage.getPageBox kind page
                addTextToRectWithScale text fontGen fontColor scale bbox arg


        let addTextToEmptySpaceOfFirstImposerRow text fontGen fontColor (scale:float) imposerTable =
            fun (arg :ManipulateArgument<_>) ->
                let page = arg.Page
                let pageBox = page.GetCropBox()

                let bbox = 
                    let margin = imposerTable.Margin
                    let firstRow = imposerTable.Rows.[0]
                    let width = pageBox.GetWidthF() - firstRow.Width - margin.Left - margin.Right
                    assert (firstRow.Width <> imposerTable.Width)
                    let height = firstRow.Height
                    let x = pageBox.GetXF() + firstRow.Width + margin.Left
                    let y = pageBox.GetYF() + (pageBox.GetHeightF() - firstRow.Height - margin.Top)
                    Rectangle.create x y width height
                addTextToRectWithScale text fontGen fontColor scale bbox arg
            
        let addLine (pos1: Position) (pos2: Position) = 
            fun arg ->
                let page = arg.Page
                let canvas = new PdfCanvas(page)
                let startPoint = pos1 |> Position.getPoint page
                let endPoint = pos2 |> Position.getPoint page
                let width = toInche 0.1 |> float32
                PdfCanvas.useCanvas canvas (fun _ ->
                    canvas.SetStrokeColorToRegistation().MoveTo(startPoint).SetLineWidth(width).LineTo(endPoint).Stroke() |> ignore
                )
                arg.FlowState
            