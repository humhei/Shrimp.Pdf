// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf.Extract

open Shrimp.Pdf
open Shrimp.FSharp.Plus
open Fake.IO
open iText.Kernel.Geom
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors



[<AutoOpen>]
module _Template_ColoredBoxes =
    type internal TargetPageBox = TargetPageBox of Rectangle option
    with 
        member x.Value =
            let (TargetPageBox v) = x
            v
    
    type IndexedBound =
        { Index: int 
          Bound: Rectangle }
    with 
        member x.ApplyMargin margin = 
            { x with Bound = Rectangle.applyMargin margin x.Bound }
    

    [<RequireQualifiedAccess>]
    type PageInfosSplitter =
        | Filter__BoundIs_InsideOrCross of Margin
        | Groupby_CenterPointIsInside
        | Groupby_DenseBoundIsInside of Margin
    with    
        

        member internal x.GetBound() =
            match x with 
            | PageInfosSplitter.Groupby_DenseBoundIsInside _ ->
                IIntegratedRenderInfoIM.getDenseBound BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageInfosSplitter.Groupby_CenterPointIsInside _ ->
                IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithoutStrokeWidth
    
            | PageInfosSplitter.Filter__BoundIs_InsideOrCross _ ->
                IIntegratedRenderInfoIM.getBound BoundGettingStrokeOptions.WithStrokeWidth
    
        static member ``Groupby_DenseBoundIsInside_MM1.5`` =
            PageInfosSplitter.Groupby_DenseBoundIsInside Margin.``MM1.5``
    
        static member Groupby_DenseBoundIsInside_MM0 =
            PageInfosSplitter.Groupby_DenseBoundIsInside Margin.Zero
    
        static member Groupby_DenseBoundIsInside_MM3 =
            PageInfosSplitter.Groupby_DenseBoundIsInside Margin.MM3
    

        static member ``Filter__BoundIs_InsideOrCross_MM1.5`` =
            PageInfosSplitter.Filter__BoundIs_InsideOrCross Margin.``MM1.5``
            
    
        member internal x.Infos__GroupOrFilter_IntoOp(bounds: Rectangle list, infos: 'info list, fBound, ?predicateEx) =
            let bounds =
                bounds
                |> List.mapi(fun i bound ->
                    { Index = i
                      Bound = bound }
                )

            let rec loop_groupBy predicate accum bounds infos =
                match bounds with 
                | [] -> List.rev accum
                | bound :: t -> 
                    let currentInfos, leftInfos =
                        infos
                        |> List.partition (fun info ->
                            let infoBound: TargetPageBox = fBound info
                            match infoBound.Value with 
                            | None -> false
                            | Some infoBound ->
                                (match predicateEx with 
                                    | Some predicateEx -> 
                                        predicateEx info bound infoBound 
                                        && predicate bound infoBound
                                    | None -> predicate bound infoBound
                                )

                                   
                        )

                    let r = 
                        match currentInfos with 
                        | [] -> None
                        | _ -> Some (bound, currentInfos)

                    loop_groupBy predicate (r :: accum) t leftInfos


            match x with 
            | PageInfosSplitter.Filter__BoundIs_InsideOrCross margin ->
                let bounds =
                    bounds
                    |> List.map (fun m -> m.ApplyMargin margin)
                    
                bounds
                |> List.mapi(fun i bound ->
                    let infos = 
                        infos
                        |> List.filter(fun info -> 
                            let infoBound: TargetPageBox = fBound info
                            match infoBound.Value with 
                            | Some infoBound -> infoBound.Is_InsideOrCross_Of(bound.Bound)
                            | None -> false
                        )
                    match infos with 
                    | [] -> None
                    | _ ->
                        Some (bound.ApplyMargin -margin, infos)
                )
    
            | PageInfosSplitter.Groupby_CenterPointIsInside ->
                loop_groupBy 
                    (fun indexedBound infoBound ->
                        infoBound.IsCenterPointInsideOf (indexedBound.Bound)
                    )
                    []
                    bounds
                    infos
     
            | PageInfosSplitter.Groupby_DenseBoundIsInside margin ->
                let bounds =
                    bounds
                    |> List.map (fun m -> m.ApplyMargin margin)
    
                loop_groupBy 
                    (fun (indexedBound: IndexedBound) infoBound ->
                        infoBound.IsInsideOf(indexedBound.Bound)
                    )
                    []
                    bounds
                    infos
                |> List.map(Option.map(fun (m, v) ->
                    m.ApplyMargin(-margin), v
                ))
            

        member internal x.Infos__GroupOrFilter_Into(bounds: Rectangle list, infos: 'info list, fBound) =
            x.Infos__GroupOrFilter_IntoOp(bounds, infos, fBound)
            |> List.choose id

    type BoxWithText =
        { Text: string list
          Box: Rectangle }
    with 
        static member Pick(pathBound: Rectangle, textInfos: IntegratedTextRenderInfo list, ?allowEmptyInsideBox: bool) =
            let textInfos =
                textInfos
                |> List.map (fun textInfo ->
                    let bound = IAbstractRenderInfo.getBound (BoundGettingStrokeOptions.WithoutStrokeWidth) textInfo
                    {|
                        Text = textInfo.Text()
                        Bound = bound
                        FsBound = bound.FsRectangle()
                    |}
            
                )


            let textInfos =
                let filteredTextInfos = 
                    textInfos
                    |> List.filter(fun textInfo ->
                        let textBound = textInfo.Bound
                        textBound.IsCenterPointInsideOf(pathBound)
                    )

                match filteredTextInfos with 
                | [textInfo] -> [textInfo]
                | [] -> 
                    match defaultArg allowEmptyInsideBox false with 
                    | true -> []
                    | false -> failwithf "No textInfo was found inside pathBound %A\nAvaiableTexts:\n%A" (pathBound.FsRectangle()) textInfos
                | textInfos -> 
                    let textInfos = 
                        textInfos
                        |> List.sortBy(fun textInfo ->
                            let bound = textInfo.Bound
                            bound.GetX(), bound.GetY()
                        )

                    textInfos

                |> List.sortBy(fun m -> m.Bound.GetX())
                    //failwithf "multiple textInfos %A were found inside pathBound %A" texts (pathBound)

            { Text = textInfos |> List.map (fun m -> m.Text)
              Box = pathBound  }

    type ColoredBoxWithTexts =
        { Text: TextTransform
          Bound: IndexedBound
          ExtractorIndex: int
          PageNumber: PageNumber
          Color: FsColor }
    with 
        member x.Box = x.Bound.Bound

        //member x.LoggingText =
        //    x.IndexTextInfo.Text() + "(" + (
        //        x.TextInfos
        //        |> List.map(fun m -> m.Text())
        //        |> String.concat " "
        //    ) + ")"

        member private x.FsRectangle = FsRectangle.OfRectangle x.Box

        static member Pick(pageNumber, color: FsColor, infos: IIntegratedRenderInfo list) =
            let bounds =
                infos
                |> List.choose IIntegratedRenderInfo.asIPathRenderInfo
                |> List.filter(fun info ->
                    IAbstractRenderInfo.ColorIs(FillOrStrokeOptions.FillOrStroke, (fun color2 ->
                        FsColor.OfItextColor color2
                        |> FsColor.equal color
                    )) info
                )
                |> List.map (IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth)
                |> Rectangle.removeInboxes

            let textInfos =
                infos
                |> List.choose IIntegratedRenderInfo.asITextRenderInfo

            let groupedBounds =
                PageInfosSplitter.Groupby_CenterPointIsInside.Infos__GroupOrFilter_IntoOp(
                    bounds,
                    textInfos,
                    fBound = (fun info ->
                        IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                        |> Some 
                        |> TargetPageBox
                    )
                ) 

            groupedBounds
            |> List.map(fun groupedBound ->
                match groupedBound with 
                | Some (bound, textInfos) ->
                    let indexedTextInfos, textInfos =
                        textInfos
                        |> List.partition(
                            IAbstractRenderInfo.FsColorIs(FillOrStrokeOptions.Fill, color)
                        )
                    { ColoredBoxWithTexts.Color = color 
                      Text = 
                        textInfos
                        |> List.map(fun m -> m.Text())
                        |> TextTransform.Create

                      Bound = bound
                      PageNumber = pageNumber
                      ExtractorIndex = indexedTextInfos |> List.exactlyOne |> fun m -> m.Text() |> System.Int32.Parse }
                    |> Some
                | None -> None
            )
            |> List.choose id
            |> List.sortBy(fun m -> m.ExtractorIndex)
            |> AtLeastOneList.TryCreate
