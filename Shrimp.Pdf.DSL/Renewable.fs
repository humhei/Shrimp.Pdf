namespace Shrimp.Pdf

open System.Collections.Generic
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors

#nowarn "0104"
open Shrimp.Pdf.Constants.Operators
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Geom
open iText.Kernel.Font
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf.Canvas.Parser.Data


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
          AccumulatedPathOperatorRanges: seq<OperatorRange>
          ClippingRule: int }
    //with 
        //member x.ApplyCtm_To_AccumulatedPathOperatorRanges() =
        //    { Ctm = x.ClippingPathCtm
        //      AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges }
        //        .ApplyCtm_To_AccumulatedPathOperatorRanges()



    type IntersectedClippingPathInfoElement with 
        member x.Renewable() =
            { ClippingPathCtm = x.Ctm 
              AccumulatedPathOperatorRanges = x.OperatorRanges
              ClippingRule = x.ClippingRule }

    //type RenewableSoftMaskRenderInfo = RenewableSoftMaskRenderInfo of SoftMaskRenderInfo
    //with 
    //    member x.Value =
    //        let (RenewableSoftMaskRenderInfo v) = x
    //        v

    //    member x.ApplyCtm_WriteToCanvas(writerCanvas: PdfCanvas) =
    //        let x = x.Value
    //        let document = writerCanvas.GetDocument() :?> PdfDocumentWithCachedResources
    //        let newSoftMask = document.Renew_OtherDocument_SoftMaskInfo(x)
                
    //        //writerCanvas
    //        //|> PdfCanvas.concatMatrixByTransformRecord x.Ctm
    //        //|> ignore

    //        writerCanvas.SetExtGState(newSoftMask)
    //        |> ignore



    //type SoftMaskRenderInfo with 
    //    member x.Renewable() = RenewableSoftMaskRenderInfo x

    type RenewableGsStateInfo = RenewableGsStateInfo of FsExtGState
    with 
        member x.Value =
            let (RenewableGsStateInfo v) = x
            v

        member x.ApplyCtm_WriteToCanvas(writerCanvas: OffsetablePdfCanvas) =
            let x = x.Value
            let document = writerCanvas.GetDocument() :?> PdfDocumentWithCachedResources

            let newExtState = document.GetOrCreateExtGState(x)
                
            //writerCanvas
            //|> PdfCanvas.concatMatrixByTransformRecord x.Ctm
            //|> ignore

            writerCanvas.SetExtGState(newExtState)
            |> ignore

    type FsExtGState with 
        member x.Renewable() =
            RenewableGsStateInfo x

    type RenewableClippingPathInfo = RenewableClippingPathInfo of RenewableClippingPathInfoElement list
    with
        member x.ApplyCtm_WriteToCanvas(canvas: OffsetablePdfCanvas, suffixActions) = 

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

                    match x.ClippingRule with 
                    | FillingRule.EVEN_ODD  -> 
                        canvas.EoClip().EndPath() |> ignore
                        //failwithf "Not implemented when clipping rule is even odd"
                    | FillingRule.NONZERO_WINDING -> canvas.Clip().EndPath() |> ignore

                    | _ -> failwithf "Invalid token"

                    


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
        


    type RenewablePathInfoTag =
        | Normal = 0
        | CuttingDie = 1

    [<RequireQualifiedAccess>]
    type RenewableModifableColor =  
        | Initial of FsColor
        | Modified of FsColor
    with 
        member x.Color =
            match x with 
            | Initial  color
            | Modified color -> color

    [<RequireQualifiedAccess>]
    module PdfCanvas =
        let private getOrCreateColor (originColor) color (pdfCanvas: PdfCanvas) =
            let color =
                match color with
                | None -> originColor
                | Some color ->
                    match color with
                    | RenewableModifableColor.Initial _ -> originColor
                    | RenewableModifableColor.Modified color ->
                        match color with 
                        | FsColor.ValueColor valueColor -> 
                            match valueColor with 
                            | FsValueColor.Lab _ ->
                                let document = pdfCanvas.GetDocument() :?> PdfDocumentWithCachedResources
                                document.GetOrCreateColor(PdfCanvasColor.OfFsColor color)

                            | _ -> valueColor.ToItextColor()
                        | _ ->
                            let document = pdfCanvas.GetDocument() :?> PdfDocumentWithCachedResources
                            document.GetOrCreateColor(PdfCanvasColor.OfFsColor color)

            color

        let setFillColor_Modifable (originFillColor) fillColor (pdfCanvas: PdfCanvas) =
            let color = getOrCreateColor originFillColor fillColor pdfCanvas
            PdfCanvas.setFillColor color pdfCanvas


        let setStrokeColor_Modifable (originStrokeColor) strokeColor (pdfCanvas: PdfCanvas) =
            let color = getOrCreateColor originStrokeColor strokeColor pdfCanvas
            PdfCanvas.setStrokeColor color pdfCanvas
             
    type private LazyColor =
        { Fill: RenewableModifableColor option  
          Stroke: RenewableModifableColor option }
    with 
        member x.LazyFillColor =
            x.Fill
            |> Option.map(fun m -> m.Color)

        member x.LazyStrokeColor =
            x.Stroke
            |> Option.map(fun m -> m.Color)

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: PdfCanvasColor) =
            match fillOrStrokeOptions with 
            | FillOrStrokeOptions.Fill ->
                match x.LazyFillColor with 
                | None -> false
                | Some fillColor -> fillColor.IsEqualTo(color)

            | FillOrStrokeOptions.Stroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color)

            | FillOrStrokeOptions.FillOrStroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color)

                || (
                    match x.LazyFillColor with 
                    | None -> false
                    | Some fillColor -> fillColor.IsEqualTo(color)
                )

            | FillOrStrokeOptions.FillAndStroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color)

                && (
                    match x.LazyFillColor with 
                    | None -> false
                    | Some fillColor -> fillColor.IsEqualTo(color)
                )

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: FsColor, ?valueEqualOptions) =
            match fillOrStrokeOptions with 
            | FillOrStrokeOptions.Fill ->
                match x.LazyFillColor with 
                | None -> false
                | Some fillColor -> fillColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)

            | FillOrStrokeOptions.Stroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)

            | FillOrStrokeOptions.FillOrStroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)

                || (
                    match x.LazyFillColor with 
                    | None -> false
                    | Some fillColor -> fillColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)
                )

            | FillOrStrokeOptions.FillAndStroke ->
                match x.LazyStrokeColor with 
                | None -> false
                | Some strokeColor -> strokeColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)

                && (
                    match x.LazyFillColor with 
                    | None -> false
                    | Some fillColor -> fillColor.IsEqualTo(color, ?valueEqualOptions = valueEqualOptions)
                )


    type RenewablePathInfo =
        { FillColor: iText.Kernel.Colors.Color 
          StrokeColor: iText.Kernel.Colors.Color
          FillShading: option<PdfName * Matrix> 
          StrokeShading: option<PdfName * Matrix> 
          Path: Path
          Ctm: Matrix
          FillingRule: int
          Operation: int
          AccumulatedPathOperatorRanges: seq<OperatorRange>
          LineShapingStyle: LineShapingStyle
          GsStates: InfoGsStateLists
          ClippingPathInfos: ClippingPathInfos
          IsShading: bool
          OriginInfo: IntegratedPathRenderInfo
          LazyFillColor_Modifiable: RenewableModifableColor option
          LazyStrokeColor_Modifiable: RenewableModifableColor option
          Tag: RenewablePathInfoTag
          LazyVisibleBound1: Rectangle option
        }
    with 
        member x.UpdateVisibleBoundWithStrokeWidth() =
            let bound = 
                match x.OriginInfo.LazyVisibleBound0 with 
                | None -> None
                | Some bound ->
                    match x.Operation with 
                    | IPathRenderInfo.Operation.HasStroke ->
                        let actualWidth = IPathRenderInfo.getActualLineWidth x.OriginInfo
                        let margin  = actualWidth.AsWidthMargin()
                        Rectangle.applyMargin margin bound
                        |> Some

                    | _ -> Some bound

            { x with LazyVisibleBound1 = bound }



        member x.CancelStroke() =
            let newOperation = 
                match x.Operation with 
                | IPathRenderInfo.Operation.HasFill -> PathRenderInfo.FILL
                | IPathRenderInfo.Operation.NoFill -> PathRenderInfo.NO_OP

            { x with Operation = newOperation }

        member x.CancelFill() =
            let newOperation = 
                match x.Operation with 
                | IPathRenderInfo.Operation.HasStroke -> PathRenderInfo.STROKE
                | IPathRenderInfo.Operation.NoStroke -> PathRenderInfo.NO_OP

            { x with Operation = newOperation }

        member x.CancelFillAndStroke() =
            { x with Operation = PathRenderInfo.NO_OP }

        member x.IsCuttingDie = 
            x.Tag = RenewablePathInfoTag.CuttingDie

        member x.ApplyStrokeStyle(strokeStyle: StrokeStyle) =
            let x = 
                { x with 
                    LineShapingStyle = 
                        { CapStyle = strokeStyle.CapStyle |> Option.defaultValue x.LineShapingStyle.CapStyle
                          JoinStyle = strokeStyle.LineJoinStyle |> Option.defaultValue x.LineShapingStyle.JoinStyle
                          DashPattern = strokeStyle.DashPattern |> Option.defaultValue x.LineShapingStyle.DashPattern
                          ActualLineWidth = 
                            match strokeStyle.Width with 
                            | None -> x.LineShapingStyle.ActualLineWidth
                            | Some width -> 
                                match x.LineShapingStyle.ActualLineWidth with 
                                | ActualLineWidth.Exactly (raw, scale) ->
                                    ActualLineWidth.Exactly(width.Value / scale, scale)
                            
                                | ActualLineWidth.Unbalance (raw, scaleX, scaleY) ->
                                    let scale = max scaleX scaleY
                                    ActualLineWidth.Exactly(width.Value / scale, scale)
                        }
                }

            match strokeStyle.ColorStyle with 
            | None -> x
            | Some colorStyle ->
                { x with 
                    Operation = 
                        match colorStyle.NullablePdfCanvasColor with
                        | Some (NullablePdfCanvasColor.N) ->
                            match x.Operation with 
                            | IPathRenderInfo.Operation.HasFill -> PathRenderInfo.FILL
                            | IPathRenderInfo.Operation.NoFill -> PathRenderInfo.NO_OP

                        | _ ->
                            match colorStyle.CloseOperator with 
                            | None -> x.Operation
                            | Some closeOperator -> 
                                match closeOperator with 
                                | CloseOperator.Open ->
                                    match x.Operation with 
                                    | IPathRenderInfo.Operation.HasFill -> IPathRenderInfo.FILLANDSTROKE
                                    | IPathRenderInfo.Operation.NoFill -> PathRenderInfo.STROKE

                                | CloseOperator.Close ->
                                    match x.Operation with 
                                    | IPathRenderInfo.Operation.HasFill -> PathRenderInfo.FILL
                                    | IPathRenderInfo.Operation.NoFill -> PathRenderInfo.NO_OP

                                | CloseOperator.Keep -> x.Operation

                    LazyStrokeColor_Modifiable =
                        match colorStyle.NullablePdfCanvasColor with 
                        | None -> x.LazyStrokeColor_Modifiable
                        | Some color ->
                            match color with 
                            | NullablePdfCanvasColor.Non -> x.LazyStrokeColor_Modifiable
                            | NullablePdfCanvasColor.PdfCanvasColor color ->
                                color.ToFsColor()
                                |> RenewableModifableColor.Modified
                                |> Some


                    GsStates =
                        match colorStyle.Opacity, colorStyle.Overprint with 
                        | None, None -> x.GsStates
                        | Some opacity, Some overprint ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetStrokeOpactity(opacity).SetStrokeOverprint(overprint)
                            )

                        | Some opacity, None ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetStrokeOpactity(opacity)

                            )

                        | None, Some overprint ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetStrokeOverprint(overprint)
                            )
                }


        member x.ApplyFillStyle(fillStyle: FillStyle) =
            match fillStyle.ColorStyle with 
            | None -> x
            | Some colorStyle ->
                { x with 
                    Operation = 
                        match colorStyle.NullablePdfCanvasColor with
                        | Some (NullablePdfCanvasColor.N) ->
                            match x.Operation with 
                            | IPathRenderInfo.Operation.HasStroke -> PathRenderInfo.STROKE
                            | IPathRenderInfo.Operation.NoStroke -> PathRenderInfo.NO_OP

                        | _ ->
                            match colorStyle.CloseOperator with 
                            | None -> x.Operation
                            | Some closeOperator -> 
                                match closeOperator with 
                                | CloseOperator.Open ->
                                    match x.Operation with 
                                    | IPathRenderInfo.Operation.HasStroke -> IPathRenderInfo.FILLANDSTROKE
                                    | IPathRenderInfo.Operation.NoStroke -> PathRenderInfo.FILL

                                | CloseOperator.Close ->
                                    match x.Operation with 
                                    | IPathRenderInfo.Operation.HasStroke -> PathRenderInfo.STROKE
                                    | IPathRenderInfo.Operation.NoStroke -> PathRenderInfo.NO_OP

                                | CloseOperator.Keep -> x.Operation

                    LazyFillColor_Modifiable =
                        match colorStyle.NullablePdfCanvasColor with 
                        | None -> x.LazyFillColor_Modifiable
                        | Some color ->
                            match color with 
                            | NullablePdfCanvasColor.Non -> x.LazyFillColor_Modifiable
                            | NullablePdfCanvasColor.PdfCanvasColor color ->
                                color.ToFsColor()
                                |> RenewableModifableColor.Modified
                                |> Some


                    GsStates =
                        match colorStyle.Opacity, colorStyle.Overprint with 
                        | None, None -> x.GsStates
                        | Some opacity, Some overprint ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetFillOpactity(opacity).SetFillOverprint(overprint)
                            )

                        | Some opacity, None ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetFillOpactity(opacity)

                            )

                        | None, Some overprint ->
                            x.GsStates.MapFsExtGsState(fun m ->
                                m.SetFillOverprint(overprint)
                            )
                }



        member x.LazyFillColor =
            x.LazyFillColor_Modifiable
            |> Option.map(fun m -> m.Color)

        member x.LazyStrokeColor =
            x.LazyStrokeColor_Modifiable
            |> Option.map(fun m -> m.Color)

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: FsColor) =
            { Fill = x.LazyFillColor_Modifiable 
              Stroke = x.LazyStrokeColor_Modifiable }.LazyColorIs(fillOrStrokeOptions, color)

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: PdfCanvasColor) =
            { Fill = x.LazyFillColor_Modifiable 
              Stroke = x.LazyStrokeColor_Modifiable }.LazyColorIs(fillOrStrokeOptions, color)


        member x.ApplyCtm_To_AccumulatedPathOperatorRanges() =
            { Ctm = x.Ctm 
              AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges }
                .ApplyCtm_To_AccumulatedPathOperatorRanges()

        member x.ApplyCtm_WriteToCanvas(canvas: OffsetablePdfCanvas) = 
            let set_inPage_extGsState() =
                match x.OriginInfo.ContainerID with 
                | [InfoContainerID.Page] -> 
                    match x.GsStates.AsList with 
                    | [gsState] -> 
                        match AtLeastOneList.TryCreate gsState.AsList with 
                        | None -> ()
                        | Some gsStates ->
                            let extState = 
                                gsStates
                                |> AtLeastOneList.map(fun m -> m.FsExtState)
                                |> FsExtGState.Concat

                            PdfCanvas.setExtGState extState canvas
                            |> ignore
                        
                    | _ -> ()
                    ()
                | _ -> ()

            PdfCanvas.useCanvas canvas (fun (canvas: OffsetablePdfCanvas) ->
                match x.IsShading with 
                | true -> 
                    let shading, ctm = 
                        [ x.FillShading
                          x.StrokeShading ]
                        |> List.choose id
                        |> List.distinct
                        |> List.exactlyOne_DetailFailingText
                    let outputStream = canvas.GetContentStream().GetOutputStream()

                    //match x.GsStates with 
                    //| None -> ()
                    //| Some extGState -> PdfCanvas.setExtGState extGState canvas |> ignore
                    set_inPage_extGsState()

                    canvas.ConcatMatrix(canvas.ApplyOffsetToCtm ctm) |> ignore

                    outputStream.WriteString(BX).Write(shading).WriteSpace().WriteString(sh).WriteSpace().WriteString(EX).WriteNewLine()
                    |> ignore

                    canvas

                | false ->
                    
                    //let operatorRanges = x.ApplyCtm_To_AccumulatedPathOperatorRanges()
                    let operatorRanges = x.AccumulatedPathOperatorRanges
                    //canvas.ConcatMatrix(AffineTransform.ofMatrix x.Ctm) |> ignore
                    set_inPage_extGsState()

                    match x.Operation with 
                    | PathRenderInfo.FILL ->
                        canvas
                        |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable

                    | IPathRenderInfo.FILLANDSTROKE ->
                        canvas
                        |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable
                        |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable

                    | PathRenderInfo.STROKE ->
                        canvas
                        |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable

                    | _ -> 
                        canvas
                        |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable
                        |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable

                    |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable
                    |> PdfCanvas.concatMatrixByTransform (canvas.ApplyOffsetToCtm(x.Ctm))
                    |> PdfCanvas.setLineShapingStyle x.LineShapingStyle
                    |> ignore

                    for operatorRange in operatorRanges do
                        PdfCanvas.writeOperatorRange operatorRange canvas |> ignore

                    PdfCanvas.closePathByOperation x.Operation x.FillingRule canvas |> ignore
                    canvas
            )

        member info.CopyToDocument(document: PdfDocumentWithCachedResources, writerResources) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color (info.FillColor)
                StrokeColor = document.Renew_OtherDocument_Color (info.StrokeColor)
                FillShading = 
                    document.Renew_OtherDocument_PdfShading(writerResources, info.FillColor)
                StrokeShading =
                    document.Renew_OtherDocument_PdfShading(writerResources, info.StrokeColor)
            }


    type IntegratedPathRenderInfo with    
        member x.Renewable() =
            let info = x.PathRenderInfo
            let gs = info.GetGraphicsState()
            //let extGState = 
            //    match x.GetAppliedExtGState() with 
            //    | EqualTo FsExtGState.DefaultValue -> None
            //    | extGState -> Some extGState
            let rule = info.GetRule()

            {
                Ctm = info.GetCtm()
                FillColor = info.GetFillColor()
                StrokeColor = info.GetStrokeColor()
                Operation = info.GetOperation()
                FillingRule = info.GetRule()
                Path = info.GetPath()
                AccumulatedPathOperatorRanges = x.AccumulatedPathOperatorRanges
                LineShapingStyle = IPathRenderInfo.getLineShapingStyle x
                ClippingPathInfos = x.ClippingPathInfos
                FillShading = None
                StrokeShading = None
                IsShading = x.IsShading
                GsStates = x.GsStates
                OriginInfo = x
                LazyFillColor_Modifiable = None
                LazyStrokeColor_Modifiable = None
                Tag = RenewablePathInfoTag.Normal
                LazyVisibleBound1 = None
            }

    [<RequireQualifiedAccess>]
    type RenewableTextInfoKind =
        | Word of string
        | ConcatedText of OperatorRange



    type RenewableTextInfo =
         { FillColor: iText.Kernel.Colors.Color 
           StrokeColor: iText.Kernel.Colors.Color
           FillShading: option<PdfName * Matrix> 
           StrokeShading: option<PdfName * Matrix> 
           LineShapingStyle: LineShapingStyle
           Kind: RenewableTextInfoKind
           RawFontSize: float32
           Font: PdfFont
           //ExtGState: FsExtGState option
           GsStates: InfoGsStateLists
           Ctm: Matrix
           FontName: DocumentFontName
           TextMatrix: Matrix
           TextLeading: float32
           TextRenderingMode: int
           WordSpacing: float32
           CharSpacing: float32
           TextRise: float32
           HorizontalScaling: float32
           HeaderTextRenderInfo: TextRenderInfo
           ClippingPathInfos: ClippingPathInfos
           OriginInfo: IntegratedTextRenderInfo
           LazyFillColor_Modifiable: RenewableModifableColor option
           LazyStrokeColor_Modifiable: RenewableModifableColor option
           }
     with 
        member x.CancelFill() =
            let newTextRenderingMode = 
                let operator: TextCloseOperator =
                    { Fill = CloseOperator.Close 
                      Stroke = CloseOperator.Keep
                      Text = None }

                operator.Apply(x.TextRenderingMode)

            { x with TextRenderingMode = newTextRenderingMode }

        member x.CancelStroke() =
            let newTextRenderingMode = 
                let operator: TextCloseOperator =
                    { Fill = CloseOperator.Keep 
                      Stroke = CloseOperator.Close
                      Text = None }

                operator.Apply(x.TextRenderingMode)

            { x with TextRenderingMode = newTextRenderingMode }

        member x.CancelFillAndStroke() =
            let newTextRenderingMode = 
                let operator: TextCloseOperator =
                    { Fill = CloseOperator.Close
                      Stroke = CloseOperator.Close
                      Text = None }

                operator.Apply(x.TextRenderingMode)

            { x with TextRenderingMode = newTextRenderingMode }

        member x.LazyFillColor =
            x.LazyFillColor_Modifiable
            |> Option.map(fun m -> m.Color)

        member x.LazyStrokeColor =
            x.LazyStrokeColor_Modifiable
            |> Option.map(fun m -> m.Color)

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: FsColor) =
            { Fill = x.LazyFillColor_Modifiable 
              Stroke = x.LazyStrokeColor_Modifiable }.LazyColorIs(fillOrStrokeOptions, color)

        member x.LazyColorIs(fillOrStrokeOptions: FillOrStrokeOptions, color: PdfCanvasColor) =
            { Fill = x.LazyFillColor_Modifiable 
              Stroke = x.LazyStrokeColor_Modifiable }.LazyColorIs(fillOrStrokeOptions, color)

        member x.ApplyCtm_WriteToCanvas(pdfCanvas: OffsetablePdfCanvas) =
            match x.FillShading, x.StrokeShading with 
            | None, None -> ()
            | _ -> failwithf "Not implemented when renewing text with shading color"

            let set_inPage_extGsState() =
                match x.OriginInfo.ContainerID with 
                | [InfoContainerID.Page] -> 
                    match x.GsStates.AsList with 
                    | [gsState] -> 
                        match AtLeastOneList.TryCreate gsState.AsList with 
                        | None -> ()
                        | Some gsStates ->
                            let extState = 
                                gsStates
                                |> AtLeastOneList.map(fun m -> m.FsExtState)
                                |> FsExtGState.Concat

                            PdfCanvas.setExtGState extState pdfCanvas
                            |> ignore
                        
                    | _ -> ()
                    ()
                | _ -> ()

            PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                let textMatrix = x.HeaderTextRenderInfo.GetTextMatrix()

                set_inPage_extGsState()

                match x.TextRenderingMode with 
                | TextRenderingMode.FILL_STROKE ->

                    pdfCanvas
                    |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable
                    |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable
                
                | TextRenderingMode.FILL ->
                    pdfCanvas
                    |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable
                    
                | TextRenderingMode.STROKE ->
                    pdfCanvas
                    |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable

                | _ ->
                    pdfCanvas
                    |> PdfCanvas.setFillColor_Modifable x.FillColor x.LazyFillColor_Modifiable
                    |> PdfCanvas.setStrokeColor_Modifable x.StrokeColor x.LazyStrokeColor_Modifiable

                |> PdfCanvas.concatMatrixByTransform (pdfCanvas.ApplyOffsetToCtm x.Ctm)
                |> PdfCanvas.setLineShapingStyle x.LineShapingStyle
     
                |> PdfCanvas.beginText
                |> PdfCanvas.setTextRenderingMode x.TextRenderingMode
                //|> PdfCanvas.setTextLeading x.TextLeading
                |> PdfCanvas.setHorizontalScaling x.HorizontalScaling
                |> PdfCanvas.setCharSpacing x.CharSpacing
                |> PdfCanvas.setWordSpacing x.WordSpacing
                |> PdfCanvas.setFontAndSize(x.Font, x.RawFontSize)
                |> PdfCanvas.setTextMatrix textMatrix
                |> (fun canvas ->
                    match x.Kind with 
                    | RenewableTextInfoKind.ConcatedText operatorRange -> PdfCanvas.writeOperatorRange operatorRange canvas
                    | RenewableTextInfoKind.Word text -> PdfCanvas.showText text canvas
                    |> ignore

                    match x.HorizontalScaling with 
                    | 100.f -> canvas
                    | scale -> canvas.SetHorizontalScaling(scale)


                )
                |> PdfCanvas.endText
            )
        
        member info.CopyToDocument(document: PdfDocumentWithCachedResources, writerResources) =
            { info with 
                FillColor = document.Renew_OtherDocument_Color (info.FillColor)
                StrokeColor = document.Renew_OtherDocument_Color (info.StrokeColor)
                Font = document.Renew_OtherDocument_Font(info.Font)
                FillShading = 
                    document.Renew_OtherDocument_PdfShading(writerResources, info.FillColor)
                StrokeShading =
                    document.Renew_OtherDocument_PdfShading(writerResources, info.StrokeColor)
            }


    type IntegratedTextRenderInfo with 
        member x.Renewable(?isWord): RenewableTextInfo =
            let renderInfo = x.TextRenderInfo
            //let gs = renderInfo.GetGraphicsState()

            //let extGState = 
            //    match x.GetAppliedExtGState() with 
            //    | EqualTo FsExtGState.DefaultValue -> None
            //    | extGState -> Some extGState

            { FillColor = x.TextRenderInfo.GetFillColor()
              StrokeColor = x.TextRenderInfo.GetStrokeColor()
              Kind = 
                match defaultArg isWord false with 
                | false -> RenewableTextInfoKind.ConcatedText(x.OperatorRange.Value)
                | true ->
                    RenewableTextInfoKind.Word(x.TextRenderInfo.GetText())
              FontName = ITextRenderInfo.getFontName x
              RawFontSize = renderInfo.GetFontSize()
              Font = renderInfo.GetFont()
              Ctm = renderInfo.GetGraphicsState().GetCtm()
              TextMatrix = renderInfo.GetTextMatrix()
              TextLeading = renderInfo.GetLeading()
              TextRenderingMode = renderInfo.GetTextRenderMode()
              HorizontalScaling = renderInfo.GetHorizontalScaling()
              WordSpacing = renderInfo.GetWordSpacing()
              CharSpacing = renderInfo.GetCharSpacing()
              TextRise = renderInfo.GetRise()
              GsStates = x.GsStates
              //ExtGState = extGState
              LineShapingStyle = ITextRenderInfo.getLineShapingStyle x
              HeaderTextRenderInfo = 
                match x.EndTextState with 
                | EndTextState.Yes -> x.ConcatedTextInfo.HeadWordInfo
                | EndTextState.Undified -> x.TextRenderInfo
                | EndTextState.No -> x.TextRenderInfo

              ClippingPathInfos = x.ClippingPathInfos
              FillShading = None
              StrokeShading = None
              OriginInfo = x
              LazyFillColor_Modifiable = None
              LazyStrokeColor_Modifiable = None
            }


    type RenewableImageInfo =
        { Image: ImageRenderInfo
          CurrentDocumentImage: CurrentDocumentImage option
          ClippingPathInfos: ClippingPathInfos
          GsStates: InfoGsStateLists
          OriginInfo: IntegratedImageRenderInfo
        }
    with 
        member x.CopyToDocument(document: PdfDocumentWithCachedResources) =
            { x with 
                CurrentDocumentImage =
                    Some (document.Renew_OtherDocument_Image(x.Image))
            }

        member x.ApplyCtm_WriteToCanvas(pdfCanvas: OffsetablePdfCanvas) =
            match x.CurrentDocumentImage with 
            | Some currentDocumentImage ->
                PdfCanvas.useCanvas pdfCanvas (fun pdfCanvas ->
                    //match x.ExtGState with 
                    //| None -> ()
                    //| Some extGState -> PdfCanvas.setExtGState extGState pdfCanvas |> ignore

                    match currentDocumentImage with 
                    | CurrentDocumentImage.XObject currentDocumentImage ->
                        pdfCanvas
                        |> PdfCanvas.concatMatrix (x.Image.GetImageCtm())
                        |> PdfCanvas.addXObject currentDocumentImage AffineTransformRecord.DefaultValue

                    | CurrentDocumentImage.Inline pdfStream ->
                        let operatorRange =
                            { Operator = PdfLiteral(EI)
                              Operands = 
                                [|
                                    pdfStream :> PdfObject
                                    PdfLiteral(EI) :> PdfObject
                                |]
                              }
                        pdfCanvas
                        |> PdfCanvas.concatMatrix (x.Image.GetImageCtm())
                        |> PdfCanvas.writeOperatorRange operatorRange
                )
                |> ignore


            | None -> failwith "CurrentDocumentImage is Empty, please CopyToDocument first"

    type IntegratedImageRenderInfo with 
        member x.Renewable(): RenewableImageInfo =
            //let extGState = 
            //    match x.GetAppliedExtGState() with 
            //    | EqualTo FsExtGState.DefaultValue -> None
            //    | extGState -> Some extGState

            { Image = x.ImageRenderInfo
              CurrentDocumentImage = None
              ClippingPathInfos = x.ClippingPathInfos
              OriginInfo = x
              GsStates = x.GsStates
            }


    [<RequireQualifiedAccess>]
    type RenewableInfo =
        | Path of RenewablePathInfo
        | Text of RenewableTextInfo
        | Image of RenewableImageInfo
    with 
        member x.UpdateVisibleBound1() =
            match x with 
            | Path info -> info.UpdateVisibleBoundWithStrokeWidth() |> Path
            | Text info -> x
            | Image _ -> x

        member x.Internal_RecoverVisibleBound_ForWrite() =
            match x with 
            | Path info -> 
                { info with 
                    OriginInfo.LazyVisibleBound0 = info.OriginInfo.LazyVisibleBound0_Backup
                    OriginInfo.LazyVisibleBound0_Backup = None }
                |> Path

            | Text info -> 
                { info with 
                    OriginInfo.LazyVisibleBound0 = info.OriginInfo.LazyVisibleBound0_Backup
                    OriginInfo.LazyVisibleBound0_Backup = None }
                |> Text


            | Image info -> 
                { info with 
                    OriginInfo.LazyVisibleBound = info.OriginInfo.LazyVisibleBound_Backup
                    OriginInfo.LazyVisibleBound_Backup = None }
                |> Image

        member x.CancelFill() =
            match x with 
            | Path info -> info.CancelFill() |> Path
            | Text info -> info.CancelFill() |> Text
            | Image _ -> x

        member x.CancelStroke() =
            match x with 
            | Path info -> info.CancelStroke() |> Path
            | Text info -> info.CancelStroke() |> Text
            | Image _ -> x

        member x.CancelFillAndStroke() =
            match x with 
            | Path info -> info.CancelFillAndStroke() |> Path
            | Text info -> info.CancelFillAndStroke() |> Text
            | Image _ -> x

        member x.IsCuttingDie =
            match x with 
            | Path info -> info.IsCuttingDie
            | _ -> false

        member x.MapPath(f) =
            match x with 
            | Path info -> f info |> Path
            | _ -> x

        member x.MapText(f) =
            match x with 
            | Text info -> f info |> Text
            | _ -> x

        member x.MapImage(f) =
            match x with 
            | Image info -> f info |> Image
            | _ -> x

        member private x.LazyVisibleBound0 = 
            match x with 
            | Path info -> info.OriginInfo.LazyVisibleBound0
            | Text info -> info.OriginInfo.LazyVisibleBound0
            | Image info -> info.OriginInfo.LazyVisibleBound

        member x.VisibleBound0 =
            match x.LazyVisibleBound0 with 
            | Some bound -> bound
            | None -> failwithf "LazyVisibleBound0 is None"

        member x.LazyVisibleBound1 =
            match x with 
            | Path info    -> info.LazyVisibleBound1
            | Text info    -> info.OriginInfo.LazyVisibleBound0
            | Image info   -> info.OriginInfo.LazyVisibleBound

        member x.VisibleBound1 =
            match x.LazyVisibleBound1 with 
            | Some bound -> bound
            | None -> failwithf "LazyVisibleBound1 is None"


        member x.MapColor(f) =
            match x with 
            | Path info ->
                { info with 
                    LazyFillColor_Modifiable = 
                        match info.LazyFillColor with 
                        | Some color -> 
                            f color
                            |> Option.map RenewableModifableColor.Modified
                        | None -> None

                    LazyStrokeColor_Modifiable = 
                        match info.LazyStrokeColor with 
                        | Some color -> 
                            f color
                            |> Option.map RenewableModifableColor.Modified

                        | None -> None
                }
                |> RenewableInfo.Path

            | Text info ->
                { info with 
                    LazyFillColor_Modifiable = 
                        match info.LazyFillColor with 
                        | Some color -> 
                            f color
                            |> Option.map RenewableModifableColor.Modified

                        | None   -> None

                    LazyStrokeColor_Modifiable = 
                        match info.LazyStrokeColor with 
                        | Some color -> 
                            f color
                            |> Option.map RenewableModifableColor.Modified

                        | None   -> None
                }
                |> RenewableInfo.Text

            | Image info -> x
            
        member x.SetColor() =
            match x with 
            | Path info ->
                { info with 
                    LazyFillColor_Modifiable = 
                        match info.LazyFillColor with 
                        | Some _ -> info.LazyFillColor_Modifiable
                        | None -> 
                            match info.Operation with 
                            | IPathRenderInfo.Operation.HasFill ->
                                FsColor.OfItextColor info.FillColor
                                |> Some
                            | IPathRenderInfo.Operation.NoFill -> None
                            |> Option.map RenewableModifableColor.Initial


                    LazyStrokeColor_Modifiable = 
                        match info.LazyStrokeColor with 
                        | Some _ -> info.LazyStrokeColor_Modifiable
                        | None ->
                            match info.Operation with 
                            | IPathRenderInfo.Operation.HasStroke ->
                                FsColor.OfItextColor info.StrokeColor
                                |> Some

                            | IPathRenderInfo.Operation.NoStroke -> None
                            |> Option.map RenewableModifableColor.Initial
                }

                |> RenewableInfo.Path

            | Text info ->
                { info with 
                    LazyFillColor_Modifiable = 
                        match info.LazyFillColor with 
                        | Some _ -> info.LazyFillColor_Modifiable
                        | None -> 
                            match info.TextRenderingMode with 
                            | TextRenderInfo.TextRenderingMode.HasFill ->
                                FsColor.OfItextColor info.FillColor
                                |> Some
                            | TextRenderInfo.TextRenderingMode.NoFill -> None
                            |> Option.map RenewableModifableColor.Initial

                    LazyStrokeColor_Modifiable = 
                        match info.LazyStrokeColor with 
                        | Some _ -> info.LazyStrokeColor_Modifiable
                        | None ->
                            match info.TextRenderingMode with 
                            | TextRenderInfo.TextRenderingMode.HasStroke ->
                                FsColor.OfItextColor info.StrokeColor
                                |> Some

                            | TextRenderInfo.TextRenderingMode.NoStroke -> None
                            |> Option.map RenewableModifableColor.Initial
                }
                |> RenewableInfo.Text

            | Image info -> x

        //member private x.Bound =
        //    match x with 
        //    | Path info ->  info.OriginInfo.RecordValue.Bound |> Some
        //    | Text info ->  info.OriginInfo.RecordValue.Bound |> Some
        //    | Image info -> info.OriginInfo.VisibleBound() |> Option.map FsRectangle.OfRectangle

        //member x.LazyBound =
        //    match x with 
        //    | Path info -> info.LazyBound
        //    | Text info -> info.LazyBound
        //    | Image info -> info.LazyBound

        //member x.SetBound(boundGettingStrokeOptions) =
        //    match x with 
        //    | Path info ->
        //        match info.LazyBound with 
        //        | None ->
        //            { info with 
        //                LazyBound = 
        //                    IAbstractRenderInfo.getBound boundGettingStrokeOptions info.OriginInfo
        //                    |> Some
        //            }

        //        | Some _ -> info
        //        |> RenewableInfo.Path

        //    | Text info ->
        //        match info.LazyBound with 
        //        | None ->
        //            { info with 
        //                LazyBound = 
        //                    IAbstractRenderInfo.getBound boundGettingStrokeOptions info.OriginInfo
        //                    |> Some
        //            }

        //        | Some _ -> info
        //        |> RenewableInfo.Text

        //    | Image info ->
        //        match info.LazyBound with 
        //        | None ->
        //            { info with 
        //                LazyBound = 
        //                    IIntegratedRenderInfoIM.getBound boundGettingStrokeOptions info.OriginInfo
        //            }

        //        | Some _ -> info
        //        |> RenewableInfo.Image



        member x.ContainerID =
            match x with 
            | Path info ->  info.OriginInfo.ContainerID
            | Text info ->  info.OriginInfo.ContainerID
            | Image info -> info.OriginInfo.ContainerID

        member x.SetContainerToPage() =
            match x with 
            | Path info ->  
                { info with 
                    OriginInfo.ContainerID = [InfoContainerID.Page] 
                }
                |> Path
            | Text info ->  
                { info with 
                    OriginInfo.ContainerID = [InfoContainerID.Page]  
                }
                |> Text

            | Image info -> 
                { info with 
                    OriginInfo.ContainerID = [InfoContainerID.Page] 
                }
                |> Image

        member x.OriginInfo =
            match x with 
            | RenewableInfo.Path info -> info.OriginInfo :> IIntegratedRenderInfoIM
            | RenewableInfo.Text info -> info.OriginInfo :> IIntegratedRenderInfoIM
            | RenewableInfo.Image info -> info.OriginInfo :> IIntegratedRenderInfoIM

        member x.GsStates =
            match x with 
            | RenewableInfo.Path info -> info.GsStates
            | RenewableInfo.Text info -> info.GsStates
            | RenewableInfo.Image info -> info.GsStates


        member info.CopyToDocument(document, writerResources) =
            match info with 
            | RenewableInfo.Path info -> 
                info.CopyToDocument(document, writerResources)
                |> RenewableInfo.Path

            | RenewableInfo.Text info -> 
                info.CopyToDocument(document, writerResources)
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


    [<RequireQualifiedAccess>]
    module RenewableInfo =
        let asText = function
            | RenewableInfo.Text v -> Some v
            | _ -> None

        let asImage = function
            | RenewableInfo.Image v -> Some v
            | _ -> None

        let asPath = function
            | RenewableInfo.Path v -> Some v
            | _ -> None

    type IntegratedRenderInfoIM with
        member info.Renewable() =
            match info with 
            | IntegratedRenderInfoIM.Vector info ->
                match info with 
                | IIntegratedRenderInfo.Path pathInfo -> 
                    let pathInfo = pathInfo.Renewable()
                    pathInfo
                    |> RenewableInfo.Path

                | IIntegratedRenderInfo.Text textInfo -> 
                    textInfo.Renewable()
                    |> RenewableInfo.Text

            | IntegratedRenderInfoIM.Pixel image ->
                image.Renewable()
                |> RenewableInfo.Image
            

namespace Shrimp.Pdf.DSL
[<AutoOpen>]
module _Modify_Renewable =
    open Shrimp.Pdf
    open Shrimp.Pdf.Colors
    
    open Shrimp.Pdf.Extensions
    open iText.Kernel.Pdf.Canvas
    

    type Modify with 
        static member internal ReadCompoundPath(selector) =
            Manipulate.Func(fun userState ->
                ModifyPage.Create
                    ("read compound path infos",
                      PageSelector.All,
                      //Selector.Path(selector),
                      Selector.Path(selector),
                      (fun args infos ->
                        let pathInfos = 
                            infos
                            |> List.ofSeq
                            |> List.choose IIntegratedRenderInfo.asIPathRenderInfo

                        pathInfos
                        |> List.map(fun pathInfo -> pathInfo.Renewable())
                      )
                    )
            )

        static member private CancelCompoundPath(pageSelector, selector) =
            Manipulate.Func(fun userState ->
                (
                    Modify.Create_Record
                        ( pageSelector,
                          [
                            { 
                                SelectorAndModifiersRecord.Name = "cancel compound paths"
                                //Selector = Selector.Path(selector)
                                Selector = Selector.Path(fun args info -> selector (args.MapUserState(fun _ -> userState)) info)
                                Modifiers =[
                                    Modifier.CancelFillAndStroke() 
                                ]
                            }
                          ]
                        )
                )
            )


        static member private CreateCompoundPathCommon(selector: PageModifingArguments<_> -> _ -> bool, options: CompoundCreatingOptions) =

            Manipulate.Func(fun userState ->
                let selector (args: PageModifingArguments<_>) info =
                    selector (args.MapUserState(fun _ -> userState)) info
                    
                (
                    match options with 
                    | CompoundCreatingOptions.CompoundPath -> 
                        Modify.ReadCompoundPath(selector)
                        <.+>
                        Modify.CancelCompoundPath(PageSelector.All, selector)
                    | CompoundCreatingOptions.ClippingPathAndCancel condition ->
                        Modify.ReadCompoundPath(selector)
                        <.+>
                        Manipulate.Func(fun infoLists ->
                            let pageNumbers =
                                infoLists
                                |> List.indexed
                                |> List.choose(fun (i, infos: RenewablePathInfo list) ->
                                    let infos = infos |> List.map(fun m -> m.Path)
                                    match condition.IsClippable infos with 
                                    | true -> Some (i+1)
                                    | false -> None
                                )

                            match pageNumbers with 
                            | [] -> Manipulate.dummy() ||>> ignore
                            | _ ->
                                let pageSelector =
                                    pageNumbers
                                    |> PageSelector.Numbers

                                Modify.CancelCompoundPath(pageSelector, selector)

                        )
                    | CompoundCreatingOptions.ClippingPathAndKeep condition ->
                        Modify.ReadCompoundPath(selector)
                )
                <+>
                (Manipulate.Func (fun (infos: RenewablePathInfo list list) ->
                    let isClippable =
                        match options with 
                        | CompoundCreatingOptions.ClippingPathAndCancel condition 
                        | CompoundCreatingOptions.ClippingPathAndKeep condition ->
                            infos
                            |> List.exists(fun m -> 
                                let infos = m |> List.map(fun m -> m.Path)
                                condition.IsClippable infos
                            )

                        | CompoundCreatingOptions.CompoundPath -> 
                            let length = infos |> List.sumBy(fun m -> m.Length)
                            length > 0

                    match isClippable with 
                    | true ->

                        ModifyPage.Create
                            ("add compound path",
                              PageSelector.All,
                              Dummy,
                              (fun (args: PageModifingArguments<RenewablePathInfo list list>) _ ->
                                let renewablePathInfos = args.UserState.[args.PageNum-1]
                                match renewablePathInfos with 
                                | [] -> ()
                                | _ ->
                                
                                    match options with 
                                    | CompoundCreatingOptions.CompoundPath ->
                                    
                                        let pdfCanvas = new OffsetablePdfCanvas(args.Page)

                                        let accumulatedPathOperatorRanges =
                                            renewablePathInfos
                                            |> List.collect(fun m -> m.ApplyCtm_To_AccumulatedPathOperatorRanges())

                                        let head = renewablePathInfos.Head

                                        for operatorRange in accumulatedPathOperatorRanges do
                                            PdfCanvas.writeOperatorRange operatorRange pdfCanvas
                                            |> ignore

                                        let doc = (args.Page.GetDocument() :?> PdfDocumentWithCachedResources)
                                        let fillColor = 
                                            doc.Renew_OtherDocument_Color(head.FillColor)

                                        let strokeColor = 
                                            doc.Renew_OtherDocument_Color(head.StrokeColor)

                                        PdfCanvas.setPathRenderColorByOperation head.Operation fillColor strokeColor pdfCanvas |> ignore
                                        pdfCanvas.EoFill() |> ignore
                                        //PdfCanvas.closePathByOperation head.Operation pdfCanvas |> ignore

                                    | CompoundCreatingOptions.ClippingPathAndCancel condition 
                                    | CompoundCreatingOptions.ClippingPathAndKeep   condition ->
                                        let isClippable =
                                            match condition with 
                                            | ClippingCondition.Always -> true
                                            | ClippingCondition.ClipIfPathCountSmallerOrEqualThan count ->
                                                renewablePathInfos.Length <= count

                                        match isClippable with 
                                        | false -> ()
                                        | true ->

                                            let pdfCanvas = new OffsetablePdfCanvas(args.Page.NewContentStreamBefore(), args.Page.GetResources(), args.Page.GetDocument())

                                            let accumulatedPathOperatorRanges =
                                                renewablePathInfos
                                                |> List.collect(fun m -> m.ApplyCtm_To_AccumulatedPathOperatorRanges())

                                            for operatorRange in accumulatedPathOperatorRanges do
                                                PdfCanvas.writeOperatorRange operatorRange pdfCanvas
                                                |> ignore

                                            pdfCanvas.EoClip().EndPath() |> ignore
                                    
                              )
                            )
                        ||>> ignore
                    | false -> Manipulate.dummy() ||>> ignore
                ))
            )



        static member CreateCompoundPath(selector: PageModifingArguments<_> -> _ -> bool) =
            Modify.CreateCompoundPathCommon(selector, options = CompoundCreatingOptions.CompoundPath)
            |> Manipulate.rename "Create Compound Path" []



        static member ReleaseCompoundPath(selector: PageModifingArguments<_> -> _ -> bool) =
            Modify.Create_Record
                (
                  PageSelector.All,
                  [
                    { SelectorAndModifiersRecord.Name = "release compound path"
                      Selector = Selector.Path selector
                      Modifiers = 
                        [
                            Modifier.ReleaseCompoundPath()
                        ]
                    }
                  ]
                )

        static member RemoveICC() =
            Modify.ReplaceColors(
                picker = (fun color ->
                    match color with 
                    | FsColor.IccBased iccBased -> iccBased.Color.ToItextColor() |> Some
                    | _ -> None
                ),
                nameAndParameters = 
                    { Name = "RemoveICC" 
                      Parameters = [] }
            )

        static member CreateClippingPath(selector: PageModifingArguments<_> -> _ -> bool, ?keepCompoundPath, ?condition) =
            let options =
                match defaultArg keepCompoundPath false with
                | false -> CompoundCreatingOptions.ClippingPathAndCancel (defaultArg condition ClippingCondition.Always)
                | true -> CompoundCreatingOptions.ClippingPathAndKeep (defaultArg condition ClippingCondition.Always)



            Modify.CreateCompoundPathCommon(selector, options)
            |> Manipulate.rename "Create Clipping Path" []



