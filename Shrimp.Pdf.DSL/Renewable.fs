namespace Shrimp.Pdf

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


[<AutoOpen>]
module _Renewable =
    
    type RenewablePathInfo =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color
          Path: Path
          Ctm: Matrix
          Operation: int
          AccumulatedPathOperatorRanges: seq<OperatorRange>
          LineWidth: float32
          DashPattern: DashPattern
          }
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
                    let transformedPoints = 
                        let rect = Rectangle.create src.[0] src.[1] src.[2] src.[3]
                        rect.ToPointsArray()
                        |> Array.map (affineTransform.Transform)
                        |> List.ofArray
                    
                    let point1 = transformedPoints.Head

                    let point1 = 
                        let point = point1
                        let operands = Array.append [|PdfNumber point.x :> PdfObject; PdfNumber point.y :> PdfObject|] [|PdfLiteral(m)|]
                        { Operands = operands :> IList<_>
                          Operator = PdfLiteral(m)
                        }

                    let tailPoints = 
                        transformedPoints.Tail
                        |> List.map(fun m -> 
                            let operands = Array.append [|PdfNumber m.x :> PdfObject; PdfNumber m.y :> PdfObject|] [|PdfLiteral(l)|]
                            { Operands = operands :> IList<_>
                              Operator = PdfLiteral(l)
                            }
                        )

                    let close = 
                        { Operands = [|PdfLiteral(h) :> PdfObject|] :> IList<_>
                          Operator = PdfLiteral(h)
                        }

                    point1 :: tailPoints @ [close]


                    //let last = Array.last operands
                    //let dst = 
                    //    affineTransform.Transform(src)
                    //    |> Array.map(fun m -> (PdfNumber m) :> PdfObject)

                    //{ operatorRange with 
                    //    Operands = Array.append dst [|last|] :> IList<_>
                    //}
                    //|> List.singleton

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

        member x.ApplyCtm_WriteToCanvas(canvas: PdfCanvas) = 
            PdfCanvas.useCanvas canvas (fun canvas ->

                let operatorRanges = x.ApplyCtm_To_AccumulatedPathOperatorRanges()
                //canvas.ConcatMatrix(AffineTransform.ofMatrix x.Ctm) |> ignore
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

        member info.CopyToDocument(document: PdfDocumentWithCachedResources) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color info.FillColor
                StrokeColor = document.Renew_OtherDocument_Color info.StrokeColor
            }


    type IntegratedPathRenderInfo with    
        member x.Renewable() =
            let info = x.PathRenderInfo
            let gs = info.GetGraphicsState()
            {
                Ctm = info.GetCtm()
                FillColor = info.GetFillColor()
                StrokeColor = info.GetStrokeColor()
                Operation = info.GetOperation()
                Path = info.GetPath()
                AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges
                LineWidth = info.GetLineWidth()
                DashPattern = CanvasGraphicsState.getDashPattern gs
            }

    type RenewableTextInfo =
         { FillColor: iText.Kernel.Colors.Color 
           StrokeColor: iText.Kernel.Colors.Color
           LineWidth: float32
           DashPattern: DashPattern
           Text: string
           RawFontSize: float32
           Font: PdfFont
           Ctm: Matrix
           TextMatrix: Matrix
           TextLeading: float32
           TextRenderingMode: int
           HeaderTextRenderInfo: TextRenderInfo
           OperatorRange: OperatorRange
     
           }
     with 
        member x.ApplyCtm_WriteToCanvas(pdfCanvas: PdfCanvas) =
            PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                let textMatrix = x.HeaderTextRenderInfo.GetTextMatrix()
     
                pdfCanvas
                |> PdfCanvas.setFillColor x.FillColor
                |> PdfCanvas.setStrokeColor x.StrokeColor
                |> PdfCanvas.setLineWidth (float x.LineWidth)
                |> PdfCanvas.setDashpattern x.DashPattern
                
                |> PdfCanvas.concatMatrix x.Ctm
     
                |> PdfCanvas.beginText
                |> PdfCanvas.setTextRenderingMode x.TextRenderingMode
                |> PdfCanvas.setTextLeading x.TextLeading
                |> PdfCanvas.setFontAndSize(x.Font, x.RawFontSize)
                |> PdfCanvas.setTextMatrix textMatrix
                |> PdfCanvas.writeOperatorRange x.OperatorRange
                |> PdfCanvas.endText
            )
        
        member info.CopyToDocument(document: PdfDocumentWithCachedResources) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color info.FillColor
                StrokeColor = document.Renew_OtherDocument_Color info.StrokeColor
                Font = document.Renew_OtherDocument_Font(info.Font)
            }


    type IntegratedTextRenderInfo with 
        member x.Renewable(): RenewableTextInfo =
            let renderInfo = x.TextRenderInfo
            { FillColor = x.TextRenderInfo.GetFillColor()
              StrokeColor = x.TextRenderInfo.GetStrokeColor()
              Text = x.Text()
              RawFontSize = renderInfo.GetFontSize()
              Font = renderInfo.GetFont()
              Ctm = renderInfo.GetGraphicsState().GetCtm()
              TextMatrix = renderInfo.GetTextMatrix()
              TextLeading = renderInfo.GetLeading()
              TextRenderingMode = renderInfo.GetTextRenderMode()
              LineWidth = renderInfo.GetGraphicsState().GetLineWidth()
              DashPattern = CanvasGraphicsState.getDashPattern (renderInfo.GetGraphicsState())
              HeaderTextRenderInfo = 
                match x.EndTextState with 
                | EndTextState.Yes -> x.ConcatedTextInfos |> Seq.head |> fun m -> m.TextRenderInfo

                | EndTextState.Undified -> x.TextRenderInfo

                | EndTextState.No -> failwith "Invalid token, EndTextState should not be 'No' here"

              OperatorRange = x.OperatorRange.Value
            }

    [<RequireQualifiedAccess>]
    type RenewableInfo =
        | Path of RenewablePathInfo
        | Text of RenewableTextInfo
    with 
        member info.CopyToDocument(document) =
            match info with 
            | RenewableInfo.Path info -> 
                info.CopyToDocument(document)
                |> RenewableInfo.Path
            | RenewableInfo.Text info -> 
                info.CopyToDocument(document)
                |> RenewableInfo.Text


        member info.ApplyCtm_WriteToCanvas(canvas) =
            match info with 
            | RenewableInfo.Path info -> 
                info.ApplyCtm_WriteToCanvas(canvas)
            | RenewableInfo.Text info -> 
                info.ApplyCtm_WriteToCanvas(canvas)


