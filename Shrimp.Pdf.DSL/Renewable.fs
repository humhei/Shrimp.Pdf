﻿namespace Shrimp.Pdf

open System.Collections.Generic

#nowarn "0104"
open Shrimp.Pdf.Constants.Operators
open Shrimp.Pdf.Extensions
open iText.Kernel.Geom
open iText.Kernel.Font
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.IO.Image


[<AutoOpen>]
module _Renewable =
    
    type private RenewablePathInfoCommon = 
        { AccumulatedPathOperatorRanges: seq<OperatorRange>
          Ctm: Matrix }
    with 
        member x.ApplyCtm_To_AccumulatedPathOperatorRanges() =
            let affineTransform = AffineTransform.ofMatrix x.Ctm
            
            x.AccumulatedPathOperatorRanges
            |> List.ofSeq
            |> List.collect(fun operatorRange ->
                let operands = Array.ofSeq operatorRange.Operands
                let operator = operatorRange.Operator.ToString()

                let src = 
                    Array.take (operands.Length-1) operands
                    |> Array.map(fun operand ->
                        (operand :?> PdfNumber).DoubleValue()
                    )

                match operator with 
                | EqualTo re ->
                    let originRect = Rectangle.create src.[0] src.[1] src.[2] src.[3]

                    let transformedPoints = 
                        originRect.ToPointsArray()
                        |> Array.map (affineTransform.Transform)
                        |> List.ofArray
                    
                    let last = Array.last operands
                    let newRect = Rectangle.ofPoints (AtLeastTwoList.Create transformedPoints)
                    let dst = 
                        let newY, newHeight = 
                            let newY = newRect.GetYF()
                            match originRect.GetHeightF() < 0., affineTransform.GetScaleY() < 0. with 
                            | true, false 
                            | false, true -> 
                                let height = newRect.GetHeightF()
                                newY + height, -(height)
                            | _ -> newY, newRect.GetHeightF()

                        let newX, newWidth = 
                            let newX = newRect.GetXF()
                            match originRect.GetWidthF() < 0., affineTransform.GetScaleX() < 0. with 
                            | true, false  
                            | false, true ->
                                let width = newRect.GetWidthF()
                                newX + width, -(width)
                            | _ -> newX, newRect.GetWidthF()

                        [|
                            newX
                            newY
                            newWidth
                            newHeight
                        |]
                        //[|
                        //    newRect.GetXF()
                        //    newRect.GetYF()
                        //    newRect.GetWidthF()
                        //    newRect.GetHeightF()
                        //|]

                        |> Array.map(fun m -> (PdfNumber m) :> PdfObject)

                    { operatorRange with 
                        Operands = Array.append dst [|last|] :> IList<_>
                    }
                    |> List.singleton

                    //let point1 = transformedPoints.Head

                    //let point1 = 
                    //    let point = point1
                    //    let operands = Array.append [|PdfNumber point.x :> PdfObject; PdfNumber point.y :> PdfObject|] [|PdfLiteral(m)|]
                    //    { Operands = operands :> IList<_>
                    //      Operator = PdfLiteral(m)
                    //    }

                    //let tailPoints = 
                    //    transformedPoints.Tail
                    //    |> List.map(fun m -> 
                    //        let operands = Array.append [|PdfNumber m.x :> PdfObject; PdfNumber m.y :> PdfObject|] [|PdfLiteral(l)|]
                    //        { Operands = operands :> IList<_>
                    //          Operator = PdfLiteral(l)
                    //        }
                    //    )



                    //let close = 
                    //    { Operands = [|PdfLiteral(h) :> PdfObject|] :> IList<_>
                    //      Operator = PdfLiteral(h)
                    //    }

                    //point1 :: tailPoints @ [close]




                | _ ->
                    let last = Array.last operands
                    let dst = 
                        affineTransform.Transform(src)
                        |> Array.map(fun m -> (PdfNumber m) :> PdfObject)

                    { operatorRange with 
                        Operands = Array.append dst [|last|] :> IList<_>
                    }
                    |> List.singleton
            )

    type RenewableClippingPathInfoElement =
        { ClippingPathCtm: Matrix 
          AccumulatedPathOperatorRanges: seq<OperatorRange> }
    with 
        member x.ApplyCtm_To_AccumulatedPathOperatorRanges() =
            { Ctm = x.ClippingPathCtm
              AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges }
                .ApplyCtm_To_AccumulatedPathOperatorRanges()



    type IntersectedClippingPathInfoElement with 
        member x.Renewable() =
            { ClippingPathCtm = x.Ctm 
              AccumulatedPathOperatorRanges = x.OperatorRanges }

    type RenewableClippingPathInfo = RenewableClippingPathInfo of RenewableClippingPathInfoElement list
    with
        member x.ApplyCtm_WriteToCanvas(canvas: PdfCanvas, suffixActions) = 

            PdfCanvas.useCanvas canvas (fun canvas ->
                
                let (RenewableClippingPathInfo elements) = x
                elements
                |> List.iter(fun x ->
                    let operatorRanges = 
                        { Ctm = x.ClippingPathCtm
                          AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges }
                            .ApplyCtm_To_AccumulatedPathOperatorRanges()

                    for operatorRange in operatorRanges do
                        PdfCanvas.writeOperatorRange operatorRange canvas |> ignore

                    canvas.Clip().EndPath() |> ignore
                )

                //canvas.ConcatMatrix(AffineTransform.ofMatrix x.Ctm) |> ignore

                suffixActions canvas


            )

        
        

    type IntersectedClippingPathInfo with 
        member x.Renewable() =
            x.Elements
            |> List.ofArray
            |> List.rev
            |> List.collect(fun m ->
                m
                |> List.ofSeq
                |> List.map(fun m -> m.Renewable())
            )
            |> RenewableClippingPathInfo
        


    type RenewablePathInfo =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color
          FillShading: option<PdfName * Matrix> 
          StrokeShading: option<PdfName * Matrix> 
          Path: Path
          Ctm: Matrix
          Operation: int
          AccumulatedPathOperatorRanges: seq<OperatorRange>
          LineWidth: float32
          DashPattern: DashPattern
          ExtGState: FsExtGState option
          ClippingPathInfos: ClippingPathInfos
          IsShading: bool
          OriginInfo: IntegratedPathRenderInfo
        }
    with 
        member x.ApplyCtm_To_AccumulatedPathOperatorRanges() =
            { Ctm = x.Ctm 
              AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges }
                .ApplyCtm_To_AccumulatedPathOperatorRanges()

        member x.ApplyCtm_WriteToCanvas(canvas: PdfCanvas) = 

            PdfCanvas.useCanvas canvas (fun canvas ->
                match x.IsShading with 
                | true -> 
                    let shading, ctm = 
                        [ x.FillShading
                          x.StrokeShading ]
                        |> List.choose id
                        |> List.distinct
                        |> List.exactlyOne_DetailFailingText
                    let outputStream = canvas.GetContentStream().GetOutputStream()

                    canvas.ConcatMatrix(AffineTransform.ofMatrix ctm) |> ignore

                    outputStream.WriteString(BX).Write(shading).WriteSpace().WriteString(sh).WriteSpace().WriteString(EX).WriteNewLine()
                    |> ignore

                    canvas

                | false ->
                    
                    let operatorRanges = x.ApplyCtm_To_AccumulatedPathOperatorRanges()
                    //canvas.ConcatMatrix(AffineTransform.ofMatrix x.Ctm) |> ignore
                    match x.ExtGState with 
                    | None -> ()
                    | Some extGState -> PdfCanvas.setExtGState extGState canvas |> ignore

                    canvas
                    |> PdfCanvas.setFillColor x.FillColor
                    |> PdfCanvas.setStrokeColor x.StrokeColor
                    |> PdfCanvas.setLineWidth (float x.LineWidth)
                    |> PdfCanvas.setDashpattern x.DashPattern
                    |> ignore

                    for operatorRange in operatorRanges do
                        PdfCanvas.writeOperatorRange operatorRange canvas |> ignore

                    PdfCanvas.closePathByOperation x.Operation canvas |> ignore
                    canvas
            )

        member info.CopyToDocument(document: PdfDocumentWithCachedResources, writerResources, readerPage) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color (readerPage, info.FillColor)
                StrokeColor = document.Renew_OtherDocument_Color (readerPage, info.StrokeColor)
                FillShading = 
                    document.Renew_OtherDocument_PdfShading(writerResources, info.FillColor)
                StrokeShading =
                    document.Renew_OtherDocument_PdfShading(writerResources, info.StrokeColor)
            }


    type IntegratedPathRenderInfo with    
        member x.Renewable() =
            let info = x.PathRenderInfo
            let gs = info.GetGraphicsState()
            let extGState = 
                match x.GetAppliedExtGState() with 
                | EqualTo FsExtGState.DefaultValue -> None
                | extGState -> Some extGState

            {
                Ctm = info.GetCtm()
                FillColor = info.GetFillColor()
                StrokeColor = info.GetStrokeColor()
                Operation = info.GetOperation()
                Path = info.GetPath()
                AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges
                LineWidth = info.GetLineWidth()
                DashPattern = CanvasGraphicsState.getDashPattern gs
                ClippingPathInfos = x.ClippingPathInfos
                FillShading = None
                StrokeShading = None
                IsShading = x.IsShading
                ExtGState = extGState
                OriginInfo = x
            }

    type RenewableTextInfo =
         { FillColor: iText.Kernel.Colors.Color 
           StrokeColor: iText.Kernel.Colors.Color
           FillShading: option<PdfName * Matrix> 
           StrokeShading: option<PdfName * Matrix> 
           LineWidth: float32
           DashPattern: DashPattern
           Text: string
           RawFontSize: float32
           Font: PdfFont
           ExtGState: FsExtGState option
           Ctm: Matrix
           FontName: DocumentFontName
           TextMatrix: Matrix
           TextLeading: float32
           TextRenderingMode: int
           HeaderTextRenderInfo: TextRenderInfo
           OperatorRange: OperatorRange option
           ClippingPathInfos: ClippingPathInfos
           OriginInfo: IntegratedTextRenderInfo
           IsWord: bool
     
           }
     with 
        member x.ApplyCtm_WriteToCanvas(pdfCanvas: PdfCanvas) =
            match x.FillShading, x.StrokeShading with 
            | None, None -> ()
            | _ -> failwithf "Not implemented when renewing text with shading color"

            PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                let textMatrix = x.HeaderTextRenderInfo.GetTextMatrix()

                match x.ExtGState with 
                | None -> ()
                | Some extGState -> PdfCanvas.setExtGState extGState pdfCanvas |> ignore


                pdfCanvas
                |> PdfCanvas.setFillColor x.FillColor
                |> PdfCanvas.setStrokeColor x.StrokeColor
                |> PdfCanvas.setLineWidth (float x.LineWidth)
                |> PdfCanvas.setDashpattern x.DashPattern
                
                |> PdfCanvas.concatMatrix x.Ctm
     
                |> PdfCanvas.beginText
                |> PdfCanvas.setTextRenderingMode x.TextRenderingMode
                //|> PdfCanvas.setTextLeading x.TextLeading
                |> PdfCanvas.setFontAndSize(x.Font, x.RawFontSize)
                |> PdfCanvas.setTextMatrix textMatrix
                |> (fun canvas ->
                    match x.IsWord with 
                    | false -> PdfCanvas.writeOperatorRange x.OperatorRange.Value canvas
                    | true -> PdfCanvas.showText x.Text canvas
                )
                |> PdfCanvas.endText
            )
        
        member info.CopyToDocument(document: PdfDocumentWithCachedResources, writerResources,  readerPage) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color (readerPage, info.FillColor)
                StrokeColor = document.Renew_OtherDocument_Color (readerPage, info.StrokeColor)
                Font = document.Renew_OtherDocument_Font(info.Font)
                FillShading = 
                    document.Renew_OtherDocument_PdfShading(writerResources, info.FillColor)
                StrokeShading =
                    document.Renew_OtherDocument_PdfShading(writerResources, info.StrokeColor)
            }


    type IntegratedTextRenderInfo with 
        member x.Renewable(?isWord): RenewableTextInfo =
            let isWord = defaultArg isWord false
            let renderInfo = x.TextRenderInfo
            let gs = renderInfo.GetGraphicsState()

            let extGState = 
                match x.GetAppliedExtGState() with 
                | EqualTo FsExtGState.DefaultValue -> None
                | extGState -> Some extGState

            { FillColor = x.TextRenderInfo.GetFillColor()
              StrokeColor = x.TextRenderInfo.GetStrokeColor()
              Text = x.Text()
              FontName = ITextRenderInfo.getFontName x
              RawFontSize = renderInfo.GetFontSize()
              Font = renderInfo.GetFont()
              Ctm = renderInfo.GetGraphicsState().GetCtm()
              TextMatrix = renderInfo.GetTextMatrix()
              TextLeading = renderInfo.GetLeading()
              TextRenderingMode = renderInfo.GetTextRenderMode()
              LineWidth = renderInfo.GetGraphicsState().GetLineWidth()
              ExtGState = extGState
              DashPattern = CanvasGraphicsState.getDashPattern (gs)
              HeaderTextRenderInfo = 
                match x.EndTextState with 
                | EndTextState.Yes -> x.ConcatedTextInfos |> Seq.head |> fun m -> m.TextRenderInfo
                | EndTextState.Undified -> x.TextRenderInfo
                | EndTextState.No -> x.TextRenderInfo

              OperatorRange = 
                match isWord with 
                | true -> None 
                | false -> Some x.OperatorRange.Value
              ClippingPathInfos = x.ClippingPathInfos
              FillShading = None
              StrokeShading = None
              OriginInfo = x
              IsWord = isWord
            }


    type RenewableImageInfo =
        { Image: ImageRenderInfo
          CurrentDocumentImage: Xobject.PdfImageXObject option
          ClippingPathInfos: ClippingPathInfos
          ExtGState: FsExtGState option
          OriginInfo: IntegratedImageRenderInfo }
    with 
        member x.CopyToDocument(document: PdfDocumentWithCachedResources) =
            { x with 
                CurrentDocumentImage =
                    Some (document.Renew_OtherDocument_Image(x.Image))
            }

        member x.ApplyCtm_WriteToCanvas(pdfCanvas: PdfCanvas) =
            match x.CurrentDocumentImage with 
            | Some currentDocumentImage ->
                PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                    match x.ExtGState with 
                    | None -> ()
                    | Some extGState -> PdfCanvas.setExtGState extGState pdfCanvas |> ignore


                    pdfCanvas
                    |> PdfCanvas.concatMatrix (x.Image.GetImageCtm())
                    |> PdfCanvas.addXObject currentDocumentImage AffineTransformRecord.DefaultValue
                )
                |> ignore


            | None -> failwith "CurrentDocumentImage is Empty, please CopyToDocument first"

    type IntegratedImageRenderInfo with 
        member x.Renewable(): RenewableImageInfo =
            let extGState = 
                match x.GetAppliedExtGState() with 
                | EqualTo FsExtGState.DefaultValue -> None
                | extGState -> Some extGState

            { Image = x.ImageRenderInfo
              CurrentDocumentImage = None
              ClippingPathInfos = x.ClippingPathInfos
              OriginInfo = x
              ExtGState = extGState }


    [<RequireQualifiedAccess>]
    type RenewableInfo =
        | Path of RenewablePathInfo
        | Text of RenewableTextInfo
        | Image of RenewableImageInfo
    with 
        member x.OriginInfo =
            match x with 
            | RenewableInfo.Path info -> info.OriginInfo :> IIntegratedRenderInfoIM
            | RenewableInfo.Text info -> info.OriginInfo :> IIntegratedRenderInfoIM
            | RenewableInfo.Image info -> info.OriginInfo :> IIntegratedRenderInfoIM

        member info.CopyToDocument(document, writerResources, readerPage: PdfPage) =
            match info with 
            | RenewableInfo.Path info -> 
                info.CopyToDocument(document, writerResources, readerPage)
                |> RenewableInfo.Path

            | RenewableInfo.Text info -> 
                info.CopyToDocument(document, writerResources, readerPage)
                |> RenewableInfo.Text

            | RenewableInfo.Image info ->
                info.CopyToDocument(document)
                |> RenewableInfo.Image

        member x.ClippingPathInfos =
            match x with 
            | RenewableInfo.Path info -> info.ClippingPathInfos
            | RenewableInfo.Text info -> info.ClippingPathInfos
            | RenewableInfo.Image info -> info.ClippingPathInfos

        member info.ApplyCtm_WriteToCanvas(canvas) =
            match info with 
            | RenewableInfo.Path info -> 
                info.ApplyCtm_WriteToCanvas(canvas)
            | RenewableInfo.Text info -> 
                info.ApplyCtm_WriteToCanvas(canvas)
            | RenewableInfo.Image info ->
                info.ApplyCtm_WriteToCanvas(canvas)


