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
    type PdfCanvasShape =
        | Rect   = 0
        | Ellipse = 1 

    type PdfCanvasAddRectangleArguments =
        { LineWidth: float 
          StrokeColor: NullablePdfCanvasColor
          FillColor: NullablePdfCanvasColor
          IsStrokeOverprint: bool
          IsFillOverprint: bool
          Shape: PdfCanvasShape
          BlendModes: BlendMode list }
    with 
        static member DefaultValue =
            { LineWidth = mm 0.1 
              StrokeColor = NullablePdfCanvasColor.BLACK
              FillColor = NullablePdfCanvasColor.N
              IsFillOverprint = false
              IsStrokeOverprint = false
              BlendModes = []
              Shape = PdfCanvasShape.Rect }
            
        member x.FsExtGState: FsExtGState option = 
            let tryAddBlendMode(v: FsExtGState option) =
                match x.BlendModes, v with 
                | [], v -> v
                | _ :: _, v ->
                    match v with 
                    | Some v -> 
                        { v with 
                            BlendModes = x.BlendModes }
                        |> Some

                    | None ->
                        { FsExtGState.DefaultValue with BlendModes = x.BlendModes }
                        |> Some

            match x.StrokeColor, x.FillColor with 
            | NullablePdfCanvasColor.Non, NullablePdfCanvasColor.Non -> None

            | NullablePdfCanvasColor.Non, NullablePdfCanvasColor.PdfCanvasColor _ -> 
                match x.IsFillOverprint with 
                | true -> Some (FsExtGState.FillOverprint)
                | false -> None

            | NullablePdfCanvasColor.PdfCanvasColor _, NullablePdfCanvasColor.Non -> 
                match x.IsStrokeOverprint with 
                | true -> Some (FsExtGState.StrokeOverprint)
                | false -> None

            | NullablePdfCanvasColor.PdfCanvasColor _, NullablePdfCanvasColor.PdfCanvasColor _ ->   
                match x.IsStrokeOverprint, x.IsFillOverprint with 
                | true, true -> Some (FsExtGState.FillStrokeOverprint)
                | true, false -> Some (FsExtGState.StrokeOverprint)
                | false, true -> Some (FsExtGState.FillOverprint)
                | false, false -> None
            |> tryAddBlendMode

    type PdfCanvasAddLineArguments =
        { LineWidth: float 
          StrokeColor: PdfCanvasColor
          DashPattern: DashPattern }
    with 
        static member DefaultValue =
            { LineWidth = mm 0.1 
              StrokeColor = PdfCanvasColor.BLACK
              DashPattern = DashPattern.Empty }

        static member DashLine(?value) =
            { LineWidth = mm 0.1 
              StrokeColor = PdfCanvasColor.BLACK
              DashPattern = 
                match value with 
                | None -> DashPattern.MM2
                | Some value -> DashPattern.Create(value) }



    type CanvasAddTextArguments = 
        { PdfFontFactory: FsPdfFontFactory 
          CanvasFontSize: CanvasFontSize 
          FontColor: PdfCanvasColor
          FontRotation: Rotation 
          Position: Position
          HorizontalTextAlignment: TextAlignment option 
          StrokeWidth: float option
          MaxFontSize: float option
          ClipContents: bool
          FsExtGState: FsExtGState option
          VerticalTextAlignment: VerticalAlignment option }

    with 
        static member DefaultValue =
            { PdfFontFactory = FsPdfFontFactory.StandardFonts (iText.IO.Font.Constants.StandardFonts.HELVETICA)
              CanvasFontSize = CanvasFontSize.Numeric 9.
              FontColor = PdfCanvasColor.BLACK
              FontRotation = Rotation.None
              Position = Position.LeftTop (0., 0.)
              HorizontalTextAlignment = None 
              StrokeWidth = None
              ClipContents = false
              FsExtGState = None
              MaxFontSize = None
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
                    | Position.Top (x, y) ->      TextAlignment.RIGHT
                    | Position.Bottom (x, y) ->   TextAlignment.LEFT


                | Rotation.Clockwise ->
                    match args.Position with 
                    | Position.YCenter (x, y) ->  TextAlignment.CENTER
                    | Position.Top (x, y) ->      TextAlignment.LEFT
                    | Position.Bottom (x, y) ->   TextAlignment.RIGHT

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
                    | Position.Left (x, y) ->    VerticalAlignment.TOP
                    | Position.Right (x, y) ->   VerticalAlignment.BOTTOM

                    
                | Rotation.Clockwise ->
                    match args.Position with 
                    | Position.XCenter (x, y) -> VerticalAlignment.MIDDLE
                    | Position.Left (x, y) ->    VerticalAlignment.BOTTOM
                    | Position.Right (x, y) ->   VerticalAlignment.TOP


    type PdfCanvas with 
        member internal x.GetOrCreateExtGState(extGState: FsExtGState) =
            let extGState =
                let doc = x.GetDocument() :?> PdfDocumentWithCachedResources
                doc.GetOrCreateExtGState(extGState)
            
            extGState

        member internal x.GetOrCreateColor(pdfCanvasColor: PdfCanvasColor) =
            let doc = x.GetDocument() :?> PdfDocumentWithCachedResources
            doc.GetOrCreateColor(pdfCanvasColor)

        static member SetStrokeColor(strokeColor: NullablePdfCanvasColor) =
            fun (canvas: PdfCanvas) ->
                match strokeColor with 
                | NullablePdfCanvasColor.PdfCanvasColor strokeColor ->
                    let color = canvas.GetOrCreateColor(strokeColor) 
                    canvas.SetStrokeColor(color)

                | NullablePdfCanvasColor.Non -> canvas


        static member SetFillColor(fillColor: NullablePdfCanvasColor) =

            fun (canvas: PdfCanvas) ->
                match fillColor with 
                | NullablePdfCanvasColor.Non -> canvas
                | NullablePdfCanvasColor.PdfCanvasColor fillColor ->
                    let color = canvas.GetOrCreateColor(fillColor) 
                    canvas.SetFillColor(color)

        member x.SetStrokeColor(strokeColor) =
            PdfCanvas.SetStrokeColor (strokeColor) x

        member x.SetFillColor(fillColor) =
            PdfCanvas.SetFillColor (fillColor) x




    [<RequireQualifiedAccess>]
    module PdfCanvas =
        let concatMatrix (matrix: Matrix) (canvas: PdfCanvas) =
            let transform = AffineTransform.ofMatrix matrix
            canvas.ConcatMatrix(transform)

        let concatMatrixByTransformRecord (transform: AffineTransformRecord) (canvas: PdfCanvas) =
            canvas.ConcatMatrix(AffineTransformRecord.toAffineTransform transform)

        let concatMatrixByTransform (transform: AffineTransform) (canvas: PdfCanvas) =
            canvas.ConcatMatrix(transform)

        let setTextMatrix (matrix: Matrix) (canvas: PdfCanvas) =
            let transform = AffineTransform.ofMatrix matrix
            canvas.SetTextMatrix(transform)

        let setTextRise (textRise) (canvas: PdfCanvas) =
            canvas.SetTextRise(textRise)

        let setWordSpacing (wordSpacing) (canvas: PdfCanvas) =
            canvas.SetWordSpacing(wordSpacing)

        let setCharSpacing (charSpacing) (canvas: PdfCanvas) =
            canvas.SetCharacterSpacing(charSpacing)


        let setHorizontalScaling (scale) (canvas: PdfCanvas) =
            canvas.SetHorizontalScaling(scale)

        let setTextLeading (leading: float32) (canvas: PdfCanvas) =
            canvas.SetLeading(leading)

        let setTextMatrixByTransform (transform: AffineTransformRecord) (canvas: PdfCanvas) =
            canvas.SetTextMatrix(AffineTransformRecord.toAffineTransform transform)

        //let setTransport (transport) (canvas: PdfCanvas) =
        //    canvas.SetExtGState

        let addLine (line: StraightLine) (mapping: PdfCanvasAddLineArguments -> PdfCanvasAddLineArguments) (canvas: PdfCanvas) =
            let args = mapping PdfCanvasAddLineArguments.DefaultValue
            let close = PdfCanvas.stroke

            canvas
            |> PdfCanvas.SetStrokeColor(NullablePdfCanvasColor.OfPdfCanvasColor args.StrokeColor)
            |> PdfCanvas.setLineWidth args.LineWidth
            |> PdfCanvas.setDashpattern args.DashPattern
            |> PdfCanvas.moveTo line.Start
            |> PdfCanvas.lineTo line.End
            |> close

        let setExtGState (extGState: FsExtGState) (canvas: PdfCanvas) =
            let extGState = canvas.GetOrCreateExtGState(extGState)
            canvas.SetExtGState(extGState)

        
        let saveState (canvas: PdfCanvas) =
            canvas.SaveState()

        let restoreState (canvas: PdfCanvas) =
            canvas.SaveState()

        let addRectangles (rects: Rectangle list) (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: PdfCanvas) =
            let args = mapping PdfCanvasAddRectangleArguments.DefaultValue
            let close =
                match args.FillColor, args.StrokeColor with 
                | NullablePdfCanvasColor.N, NullablePdfCanvasColor.N -> PdfCanvas.endPath
                | _, NullablePdfCanvasColor.N -> PdfCanvas.fill
                | NullablePdfCanvasColor.N , _ -> PdfCanvas.stroke
                | _, _ -> PdfCanvas.fillStroke

            let trySetExtGState canvas = 
                match args.FsExtGState with 
                | Some extGState -> setExtGState extGState canvas
                | None -> canvas

            canvas
            |> trySetExtGState
            |> PdfCanvas.SetStrokeColor args.StrokeColor
            |> PdfCanvas.SetFillColor args.FillColor
            |> PdfCanvas.setLineWidth args.LineWidth
            |> fun canvas ->
                for rect in rects do 
                    match args.Shape with 
                    | PdfCanvasShape.Rect ->
                        PdfCanvas.rectangle rect canvas
                        |> ignore

                    | PdfCanvasShape.Ellipse -> 
                        PdfCanvas.ellipse rect canvas
                        |> ignore
                        

                canvas

            |> close

        let addRectangle (rect: Rectangle) (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: PdfCanvas) =
            let mapping = fun args -> 
                { mapping args with Shape = PdfCanvasShape.Rect }

            addRectangles [rect] mapping canvas

        let addEllipse (rect: Rectangle) (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: PdfCanvas) =
            let mapping = fun args -> 
                { mapping args with Shape = PdfCanvasShape.Ellipse }

            addRectangles [rect] mapping canvas


            

    type PdfCanvas with 
        member x.SetExtGState(fsExtGSState) =
            PdfCanvas.setExtGState fsExtGSState x

    type Canvas with 
        member internal x.GetOrCreateColor(pdfCanvasColor: PdfCanvasColor) =
            x.GetPdfCanvas().GetOrCreateColor(pdfCanvasColor)

        member canvas.CalcFontSize (text, args: CanvasAddTextArguments) = 
            let pdfFontFactory, canvasFontSize = 
                args.PdfFontFactory, args.CanvasFontSize

            let pdfFont =
                let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                pdfDocument.GetOrCreatePdfFont(pdfFontFactory)
            
            let redirectByMaxFontSize maxFontSize fontSize =
                match maxFontSize with 
                | None -> fontSize
                | Some maxFontSize ->
                    min fontSize maxFontSize

            match canvasFontSize with 
            | CanvasFontSize.Numeric size -> size
            | CanvasFontSize.OfRootAreaCase (scale, maxFontSize) ->
                let area = canvas.GetRootArea()
                PdfFont.fontSizeOfArea area text pdfFont * scale
                |> redirectByMaxFontSize maxFontSize 

            | CanvasFontSize.OfFsAreaCase (area, scale, maxFontSize) ->
                PdfFont.fontSizeOfArea area.AsRectangle text pdfFont * scale
                |> redirectByMaxFontSize maxFontSize 




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

                let fontColor = canvas.GetOrCreateColor(fontColor)
                    
                let pdfFont =
                    let pdfDocument = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
                    pdfDocument.GetOrCreatePdfFont(pdfFontFactory)

                let fontSize = canvas.CalcFontSize(text, args)

                let rootArea = canvas.GetRootArea()
                let point =
                    rootArea.GetPoint(position)

                let horizonal = args.GetCalculatedHorizontalTextAlignment()

                let vertical = args.GetCalculatedVerticalTextAlignment()

                let clipped = args.ClipContents
                match clipped with 
                | true -> 
                    canvas.GetPdfCanvas()
                        .SaveState()
                        .Rectangle(rootArea)
                        .EoClip()
                        .EndPath()
                        |> ignore

                | false -> ()

                let canvas =
                    let canvas =
                        match args.StrokeWidth with 
                        | None -> canvas
                        | Some strokeWidth ->
                            canvas
                                .SetTextRenderingMode(TextRenderingMode.FILL_STROKE)
                                .SetStrokeWidth(float32 strokeWidth)
                                .SetStrokeColor(fontColor)
                    
                    match args.FsExtGState with 
                    | None -> ()
                    | Some extGsState ->
                        canvas.GetPdfCanvas()
                        |> PdfCanvas.setExtGState extGsState
                        |> ignore
                    
                    canvas
                        .SetFont(pdfFont)
                        .SetFontColor(fontColor)
                        .SetFontSize(float32 fontSize)
                        .ShowTextAligned(text, float32 point.x, float32 point.y, Nullable(horizonal), Nullable(vertical), float32 (Rotation.getRadians fontRotation) )
                        

                match clipped with 
                | true -> canvas.GetPdfCanvas().RestoreState() |> ignore
                | false -> ()

                canvas


        let addRectangle rect (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: Canvas) =
            PdfCanvas.addRectangle rect mapping (canvas.GetPdfCanvas()) |> ignore
            canvas



        let addMark (mark: Mark) (position) (canvas: Canvas) =
            let document = canvas.GetPdfDocument() :?> PdfDocumentWithCachedResources
            let pdfFormObject = document.GetOrCreateXObject(Mark.obtainPdfFile mark)
            //let pagebox = canvas.GetPage().GetPageBox(PageBoxKind.ActualBox)
            let affineTransform: AffineTransform = 
                let rootArea = canvas.GetRootArea()
                let xobjectBBox = pdfFormObject |> PdfFormXObject.getActualBox

                let xobjectArea = rootArea.GetArea(position, xobjectBBox.GetWidthF(), xobjectBBox.GetHeightF())
                AffineTransform.GetTranslateInstance(xobjectArea.GetXF(), xobjectArea.GetYF())

            canvas.GetPdfCanvas().AddXObject(pdfFormObject, affineTransform)
            |> ignore

            canvas



        let addRectangleToRootArea (mapping: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments) (canvas: Canvas) =
            PdfCanvas.addRectangle (canvas.GetRootArea()) mapping (canvas.GetPdfCanvas()) |> ignore
            canvas


