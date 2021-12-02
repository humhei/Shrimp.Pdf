namespace Shrimp.Pdf.Extensions
#nowarn "0104"
open System
open iText.Layout
open iText.Kernel.Colors
open iText.Layout.Properties
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Pdf
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom

[<AutoOpen>]
module PdfDocumentWithCachedResources =

    type PdfCanvasAddRectangleArguments =
        { LineWidth: float 
          StrokeColor: PdfCanvasColor
          FillColor: PdfCanvasColor }
    with 
        static member DefaultValue =
            { LineWidth = mm 0.1 
              StrokeColor = PdfCanvasColor.ITextColor (DeviceGray.BLACK :> Color)
              FillColor = PdfCanvasColor.N }


    type PdfCanvasAddLineArguments =
        { LineWidth: float 
          StrokeColor: PdfCanvasColor
          DashPattern: DashPattern }
    with 
        static member DefaultValue =
            { LineWidth = mm 0.1 
              StrokeColor = PdfCanvasColor.ITextColor (DeviceGray.BLACK :> Color)
              DashPattern = DashPattern.Empty }

        static member DashLine(?value) =
            { LineWidth = mm 0.1 
              StrokeColor = PdfCanvasColor.ITextColor (DeviceGray.BLACK :> Color)
              DashPattern = DashPattern.Create(defaultArg value (mm 2.0)) }


    type CanvasAddTextArguments = 
        { PdfFontFactory: FsPdfFontFactory 
          CanvasFontSize: CanvasFontSize 
          FontColor: PdfCanvasColor
          FontRotation: Rotation 
          Position: Position
          HorizontalTextAlignment: TextAlignment option 
          VerticalTextAlignment: VerticalAlignment option }

    with 
        static member DefaultValue =
            { PdfFontFactory = FsPdfFontFactory.StandardFonts (iText.IO.Font.Constants.StandardFonts.HELVETICA)
              CanvasFontSize = CanvasFontSize.Numeric 9.
              FontColor = PdfCanvasColor.ITextColor DeviceGray.BLACK 
              FontRotation = Rotation.None
              Position = Position.LeftTop (0., 0.)
              HorizontalTextAlignment = None 
              VerticalTextAlignment = None }

        member args.GetCalculatedHorizontalTextAlignment() =
            match args.HorizontalTextAlignment with 
            | Some textAlignment -> textAlignment
            | None ->
                match args.FontRotation with 
                | Rotation.None ->
                    match args.Position with 
                    | Position.XCenter (x, y) -> TextAlignment.CENTER
                    | Position.Left (x, y) -> TextAlignment.LEFT
                    | Position.Right (x, y) -> TextAlignment.RIGHT

                | Rotation.R180 ->
                    match args.Position with 
                    | Position.XCenter (x, y) -> TextAlignment.CENTER
                    | Position.Left (x, y) ->    TextAlignment.RIGHT
                    | Position.Right (x, y) ->   TextAlignment.LEFT

                | Rotation.Counterclockwise ->
                    match args.Position with 
                    | Position.YCenter (x, y) ->  TextAlignment.CENTER
                    | Position.Top (x, y) ->      TextAlignment.LEFT
                    | Position.Bottom (x, y) ->   TextAlignment.RIGHT


                | Rotation.Clockwise ->
                    match args.Position with 
                    | Position.YCenter (x, y) ->  TextAlignment.CENTER
                    | Position.Top (x, y) ->      TextAlignment.RIGHT
                    | Position.Bottom (x, y) ->   TextAlignment.LEFT

        member args.GetCalculatedVerticalTextAlignment() =
            match args.VerticalTextAlignment with 
            | Some verticalAlignment -> verticalAlignment
            | None ->
                match args.FontRotation with 
                | Rotation.None ->
                    match args.Position with 
                    | Position.YCenter (x, y) -> VerticalAlignment.MIDDLE
                    | Position.Top (x, y) -> VerticalAlignment.TOP
                    | Position.Bottom (x, y) -> VerticalAlignment.BOTTOM

                | Rotation.R180 ->
                    match args.Position with 
                    | Position.YCenter (x, y) -> VerticalAlignment.MIDDLE
                    | Position.Top (x, y) ->     VerticalAlignment.BOTTOM
                    | Position.Bottom (x, y) ->  VerticalAlignment.TOP

                | Rotation.Counterclockwise ->
                    match args.Position with 
                    | Position.XCenter (x, y) -> VerticalAlignment.MIDDLE
                    | Position.Left (x, y) ->    VerticalAlignment.BOTTOM
                    | Position.Right (x, y) ->   VerticalAlignment.TOP

                    
                | Rotation.Clockwise ->
                    match args.Position with 
                    | Position.XCenter (x, y) -> VerticalAlignment.MIDDLE
                    | Position.Left (x, y) ->    VerticalAlignment.TOP
                    | Position.Right (x, y) ->   VerticalAlignment.BOTTOM


    type PdfCanvas with 
        member internal x.GetOrCreateColor(pdfCanvasColor: PdfCanvasColor) =
            match pdfCanvasColor with 
            | PdfCanvasColor.Value color -> color |> FsValueColor.ToItextColor |> Some
            | PdfCanvasColor.Separation separation ->
                let pdfDocument = x.GetDocument() :?> PdfDocumentWithCachedResources
                let resourceColor = ResourceColor.CustomSeparation separation
                pdfDocument.GetOrCreateColor(resourceColor) 
                |> Some

            | PdfCanvasColor.Lab lab ->
                let pdfDocument = x.GetDocument() :?> PdfDocumentWithCachedResources
                let resourceColor = ResourceColor.Lab lab
                pdfDocument.GetOrCreateColor(resourceColor) 
                |> Some
                
            | PdfCanvasColor.N -> None
            | PdfCanvasColor.ColorCard colorCard ->
                match colorCard with 
                | ColorCard.KnownColor knownColor ->
                    DeviceRgb.fromKnownColor knownColor
                    :> Color
                    |> Some
                | ColorCard.Pantone pantoneColor ->
                    let pdfDocument = x.GetDocument() :?> PdfDocumentWithCachedResources
                    let resourceColor = ResourceColor.Pantone pantoneColor
                    pdfDocument.GetOrCreateColor(resourceColor) 
                    |> Some
                | ColorCard.TPX tpxColor ->
                    let pdfDocument = x.GetDocument() :?> PdfDocumentWithCachedResources
                    let resourceColor = ResourceColor.Tpx tpxColor
                    pdfDocument.GetOrCreateColor(resourceColor) 
                    |> Some

            | PdfCanvasColor.Registration ->
                    let pdfDocument = x.GetDocument() :?> PdfDocumentWithCachedResources
                    let resourceColor = ResourceColor.Registration
                    pdfDocument.GetOrCreateColor(resourceColor) 
                    |> Some


        static member SetStrokeColor(strokeColor: PdfCanvasColor) =
            fun (canvas: PdfCanvas) ->
                match canvas.GetOrCreateColor(strokeColor) with 
                | Some color ->canvas.SetStrokeColor(color)
                | None -> canvas

        static member SetFillColor(fillColor: PdfCanvasColor) =
            fun (canvas: PdfCanvas) ->
                match canvas.GetOrCreateColor(fillColor) with 
                | Some color ->canvas.SetFillColor(color)
                | None -> canvas

        member x.SetStrokeColor(strokeColor: PdfCanvasColor) =
            PdfCanvas.SetStrokeColor (strokeColor) x

        member x.SetFillColor(fillColor: PdfCanvasColor) =
            PdfCanvas.SetFillColor (fillColor) x



    [<RequireQualifiedAccess>]
    module PdfCanvas =
        let addLine (line: StraightLine) (mapping: PdfCanvasAddLineArguments -> PdfCanvasAddLineArguments) (canvas: PdfCanvas) =
            let args = mapping PdfCanvasAddLineArguments.DefaultValue
            let close =
                match args.StrokeColor with 
                | PdfCanvasColor.N -> PdfCanvas.endPath
                | _ -> PdfCanvas.stroke
            
            canvas
            |> PdfCanvas.SetStrokeColor(args.StrokeColor)
            |> PdfCanvas.setLineWidth args.LineWidth
            |> PdfCanvas.setDashpattern args.DashPattern
            |> PdfCanvas.moveTo line.Start
            |> PdfCanvas.lineTo line.End
            |> close


        let addRectangle (rect: Rectangle) (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: PdfCanvas) =
            let args = mapping PdfCanvasAddRectangleArguments.DefaultValue
            let close =
                match args.FillColor, args.StrokeColor with 
                | PdfCanvasColor.N, PdfCanvasColor.N -> PdfCanvas.endPath
                | _, PdfCanvasColor.N -> PdfCanvas.fill
                | PdfCanvasColor.N , _ -> PdfCanvas.stroke
                | _, _ -> PdfCanvas.fillStroke

            canvas
            |> PdfCanvas.SetStrokeColor args.StrokeColor
            |> PdfCanvas.SetFillColor args.FillColor
            |> PdfCanvas.setLineWidth args.LineWidth
            |> PdfCanvas.rectangle rect
            |> close

    type Canvas with 
        member internal x.GetOrCreateColor(pdfCanvasColor: PdfCanvasColor) =
            x.GetPdfCanvas().GetOrCreateColor(pdfCanvasColor)

        member canvas.CalcFontSize (text, args: CanvasAddTextArguments) = 
            let pdfFontFactory, canvasFontSize = 
                args.PdfFontFactory, args.CanvasFontSize

            let pdfFont =
                let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                pdfDocument.GetOrCreatePdfFont(pdfFontFactory)
            
            match canvasFontSize with 
            | CanvasFontSize.Numeric size -> size
            | CanvasFontSize.OfRootArea (scale) ->
                let area = canvas.GetRootArea()
                PdfFont.fontSizeOfArea area text pdfFont * scale

            | CanvasFontSize.OfArea (area) ->
                PdfFont.fontSizeOfArea area text pdfFont

        member canvas.CalcTextLineWidthUnits (text, args: CanvasAddTextArguments) = 
            let pdfFont = 
                let pdfFontFactory = 
                    args.PdfFontFactory
                let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                pdfDocument.GetOrCreatePdfFont(pdfFontFactory)

            PdfFont.calcLineWidthUnits text pdfFont


        member canvas.CalcTextWidth (text, args: CanvasAddTextArguments) = 
            let fontSize = canvas.CalcFontSize(text, args)
            let widthUnit = 
                let widthUnits = canvas.CalcTextLineWidthUnits(text, args)
                widthUnits |> List.max

            fontSize * widthUnit
           

    [<RequireQualifiedAccess>]
    module Canvas =

        let addText
            text
            (mapping: CanvasAddTextArguments -> CanvasAddTextArguments)
            (canvas: Canvas) = 
                let args = mapping CanvasAddTextArguments.DefaultValue
                let pdfFontFactory, fontColor, fontRotation, position = 
                    args.PdfFontFactory, args.FontColor, args.FontRotation, args.Position

                match canvas.GetOrCreateColor(fontColor) with 
                | Some fontColor ->

                    let pdfFont =
                        let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                        pdfDocument.GetOrCreatePdfFont(pdfFontFactory)

                    let fontSize = canvas.CalcFontSize(text, args)

                    let point =
                        let rootArea = canvas.GetRootArea()
                        rootArea.GetPoint(position)

                    let horizonal = args.GetCalculatedHorizontalTextAlignment()

                    let vertical = args.GetCalculatedVerticalTextAlignment()

                    let canvas =
                        canvas
                            .SetFont(pdfFont)
                            .SetFontColor(fontColor)
                            .SetFontSize(float32 fontSize)
                            .ShowTextAligned(text,float32 point.x,float32 point.y, Nullable(horizonal), Nullable(vertical), float32 (Rotation.getRadians fontRotation) )

                    canvas

                | None -> canvas




        let addRectangleToRootArea (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: Canvas) =
            PdfCanvas.addRectangle (canvas.GetRootArea()) mapping (canvas.GetPdfCanvas()) |> ignore
            canvas
