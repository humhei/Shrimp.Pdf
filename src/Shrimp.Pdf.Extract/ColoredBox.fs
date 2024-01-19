// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf.Extract
#nowarn "0104"
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
    type TargetPageBox = TargetPageBox of option<PageBoxKind * Rectangle>
    with 
        member x.Value =
            let (TargetPageBox (v)) = x
            v

        static member create pageBoxKind (rect: Rectangle option) =
            match rect with 
            | None -> TargetPageBox None
            | Some rect -> TargetPageBox(Some (pageBoxKind, rect))

        static member Create (rect: Rectangle option) =
            match rect with 
            | None -> TargetPageBox None
            | Some rect -> TargetPageBox(Some (PageBoxKind.AllBox, rect))

    
    type SerializableIndexedBound =
        { Index: int 
          Bound: FsRectangle }




    /// Zero indexed
    type IndexedBound =
        { Index: int 
          Bound: Rectangle }
    with 
        member x.ApplyMargin margin = 
            { x with Bound = Rectangle.applyMargin margin x.Bound }
        
        member x.Serializable =
            { SerializableIndexedBound.Index = x.Index
              Bound = FsRectangle.OfRectangle x.Bound }



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
            
    
        member x.Infos__GroupOrFilter_IntoOp(bounds: Rectangle list, infos: 'info list, fBound, ?predicateEx) =
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
                            | Some (_, infoBound) ->
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
                            | Some (_, infoBound) -> infoBound.Is_InsideOrCross_Of(bound.Bound)
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


    [<RequireQualifiedAccess>]
    module private TextInfos =
        let sortByRotation fBound fRotation textInfos =
            let rotations = 
                textInfos
                |> List.map(fun m -> fRotation m)
                |> List.distinct
 
            match rotations with 
            | [rotation] ->
                let textInfos = 
                    textInfos
                    |> List.sortBy(fun textInfo ->
                        let bound: Rectangle = fBound textInfo
                        match rotation with 
                        | Rotation.None -> NearbyPX(-bound.GetYF()), NearbyPX(bound.GetXF())
                        | Rotation.R180 -> 
                            NearbyPX(-bound.GetYF()), NearbyPX(-bound.GetXF())

                        | Rotation.Counterclockwise ->
                            NearbyPX(bound.GetXF()), NearbyPX(bound.GetYF())

                        | Rotation.Clockwise ->
                            NearbyPX(bound.GetXF()), NearbyPX(-bound.GetYF())
                    )
                textInfos
            | _ ->
                textInfos


    type BoxWithText =
        { /// contents inside box
          Text: PdfConcatedTexts
          Box: Rectangle }
    with 
        static member Pick(pathBound: Rectangle, textInfos: IntegratedTextRenderInfo list, ?allowEmptyInsideBox: bool) =
            let textInfos =
                textInfos
                |> List.map (fun textInfo ->
                    let bound = IAbstractRenderInfo.getDenseBound (BoundGettingStrokeOptions.WithoutStrokeWidth) textInfo
                    {|
                        Text = textInfo.PdfConcatedWord()
                        Bound = bound
                        FsBound = bound.FsRectangle()
                        Rotation = ITextRenderInfo.getTextRotation textInfo
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
                    TextInfos.sortByRotation (fun m -> m.Bound) (fun m -> m.Rotation) textInfos

                    //failwithf "multiple textInfos %A were found inside pathBound %A" texts (pathBound)

            { Text = textInfos |> List.map (fun m -> m.Text) |> PdfConcatedTexts.Words
              Box = pathBound  }

        
    [<RequireQualifiedAccess>]
    type TextPosition =
        | InsideBox
        | BothInsideAndOutsideBox

    type ColoredBoxFontAppearance =
        { FontName: FsFontName option 
          Size: float
          Color: FsColor
          StrokeWidth: float option }

    with 
        static member internal Read(textInfos: IntegratedTextRenderInfo list) =
            let fill = 
                textInfos
                |> List.tryFind(ITextRenderInfo.hasFill)

            match fill with 
            | None -> None
            | Some fill ->

                let stroke = 
                    textInfos
                    |> List.tryFind(ITextRenderInfo.hasStroke)

                let fontName =
                    fill.TextRenderInfo.GetFont().GetFontProgram().GetFontNames()
                    |> FsFontName.TryCreate
                let border =
                    match stroke with 
                    | Some v ->
                        v.TextRenderInfo.GetGraphicsState().GetLineWidth()
                        |> float
                        |> Some
                    | _ -> None 

                { Color = fill.TextRenderInfo.GetFillColor() |> FsColor.OfItextColor
                  Size = ITextRenderInfo.getActualFontSize fill 
                  FontName = fontName
                  StrokeWidth = border
                }
                |> Some

    type ColoredBoxWithText =
        { 
          /// contents inside bound
          Text: PdfConcatedTexts
          Bound: SerializableIndexedBound
          /// Only exists when ColoredBoxWithText.Pick(extractingIndex = true)
          FontAppearance: ColoredBoxFontAppearance option
          ExtractorIndex: int option
          PageNumber: PageNumber
          Color: FsColor }
    with 
        member x.Box = x.Bound.Bound.AsRectangle

        static member Pick(pageNumber, color: FsColor, infos: IIntegratedRenderInfo list, ?extractingIndex, ?specificIndexColor) =
            let extractingIndex = defaultArg extractingIndex false

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
                        |> TargetPageBox.Create
                    )
                ) 

            groupedBounds
            |> List.map(fun groupedBound ->
                match groupedBound with 
                | Some (bound, textInfos) ->

                    let textInfos, index =
                        match extractingIndex with 
                        | false -> textInfos, None
                        | true  -> 
                            let color =
                                match specificIndexColor with 
                                | None -> color
                                | Some color -> color 
                            let indexedTextInfos, textInfos =
                                textInfos
                                |> List.partition(
                                    IAbstractRenderInfo.FsColorIs(FillOrStrokeOptions.Fill, color)
                                )
                            let index = 
                                match extractingIndex with 
                                | false -> None
                                | true ->
                                    match indexedTextInfos with 
                                    | [] -> None 
                                    | _ -> 
                                        match indexedTextInfos  with 
                                        | [v] -> v.ConcatedText() |> System.Int32.Parse |> Some
                                        | _ ->
                                            indexedTextInfos 
                                            |> List.map (fun m -> m.ConcatedText())
                                            |> List.map (System.Int32.Parse)
                                            |> List.distinct
                                            |> List.exactlyOne_DetailFailingText
                                            |> Some


                            textInfos, index

                    let fonts = 
                        match extractingIndex with 
                        | true -> ColoredBoxFontAppearance.Read textInfos
                        | false -> None

                    { ColoredBoxWithText.Color = color 
                      Text = 
                        textInfos
                        |> TextInfos.sortByRotation (fun _ -> bound.Bound) (ITextRenderInfo.getTextRotation)
                        |> List.map(fun m -> m.PdfConcatedWord())
                        |> PdfConcatedTexts.Words

                      FontAppearance = fonts
                      Bound = bound.Serializable
                      PageNumber = pageNumber
                      ExtractorIndex = index
                      }
                    |> Some
                | None -> None
            )
            |> List.choose id
            |> List.sortBy(fun m -> m.ExtractorIndex)
            |> AtLeastOneList.TryCreate

        member private x.FsRectangle = x.Bound.Bound


    type ColoredBoxWithNumberAndText = 
        { 
          /// contents inside bound
          Text: PdfConcatedTexts
          Bound: SerializableIndexedBound
          FontAppearance: ColoredBoxFontAppearance option
          ExtractorIndex: int
          PageNumber: PageNumber
          Color: FsColor }
    with 
        member private x.ConcatedText =  x.Text.ConcatedText()

        member x.Number  =x.ExtractorIndex

        member x.Box = x.Bound.Bound.AsRectangle

        static member Pick(pageNumber, color: FsColor, infos: IIntegratedRenderInfo list, ?specificIndexColor) =

            let x =     
                ColoredBoxWithText.Pick(
                    pageNumber,
                    color,
                    infos,
                    extractingIndex = true,
                    ?specificIndexColor = specificIndexColor
                )
            x
            |> Option.map (AtLeastOneList.map(fun m ->
            
                { Text = m.Text 
                  Bound = m.Bound
                  FontAppearance = m.FontAppearance
                  ExtractorIndex = 
                    match m.ExtractorIndex with 
                    | Some v -> v
                    | None -> failwithf "Cannot find Index Text in %A" (m)
                  PageNumber = m.PageNumber
                  Color = m.Color}
            ))

    


    type _OutsidesAndInsides<'OutSide, 'Inside> = 
        { 
            Outsides: 'OutSide list
            Insides:  'Inside  list
        }

    /// No contents
    type ColoredBoxWithNumber =
        { 
          Bound: SerializableIndexedBound
          ExtractorIndex: int
          PageNumber: PageNumber
          Color: FsColor }


        member x.Number = x.ExtractorIndex

        member x.Box = x.Bound.Bound.AsRectangle




        static member Pick(pageNumber, color, infos_insides) =
            ColoredBoxWithNumberAndText.Pick(pageNumber, color, infos_insides)
            |> Option.map (AtLeastOneList.map(fun m ->
                { Bound = m.Bound
                  Color  = m.Color
                  ExtractorIndex = m.ExtractorIndex
                  PageNumber = m.PageNumber}
                
            ))

        
        static member Selector(color: FsColor, textPosition, ?specificIndexColor) =
            let colors =
                [
                    color
                    match specificIndexColor with 
                    | Some color -> color
                    | None -> ()
                ]

            PathOrText(
                fun args info ->
                    let textPositionPredicate = 
                        match textPosition with 
                        | TextPosition.InsideBox ->
                            Info.BoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox) args info
                        | TextPosition.BothInsideAndOutsideBox -> true

                    textPositionPredicate
                    && Info.ColorIsOneOf(FillOrStrokeOptions.FillOrStroke, colors) args info
            )

        static member private Flow_Read_PageModifier(color: FsColor, textPosition) =
            (fun args infos ->  
                let pageBox = args.Page.GetActualBox()
                let infos_insides, infos_outsides =
                    infos
                    |> List.ofSeq
                    |> List.partition(fun info ->
                        let bound = 
                            IAbstractRenderInfo.getBound 
                                BoundGettingStrokeOptions.WithoutStrokeWidth
                                info
                        bound.IsInsideOf(pageBox)
                    )


                let insides = 
                    ColoredBoxWithNumber.Pick(PageNumber args.PageNum, color, infos_insides)
                    |> AtLeastOneList.optionToList

                let outSides =
                    match textPosition with 
                    | TextPosition.InsideBox _ -> None
                    | TextPosition.BothInsideAndOutsideBox _ -> 
                        ColoredBoxWithNumber.Pick(PageNumber args.PageNum, color, infos_outsides)
                    |> AtLeastOneList.optionToList

                {
                    Insides = insides
                    Outsides = outSides
                }

            )

        static member Flow_Read(color: FsColor, textPosition) =
            let flow =
                ModifyPage.Create(
                    "read ColoredBoxWithNumber",
                    PageSelector.All,
                    ColoredBoxWithNumber.Selector(color, textPosition),
                    ColoredBoxWithNumber.Flow_Read_PageModifier(color, textPosition)
                )

            flow


    type ColoredBoxWithNumberAndTextFactory =
        static member Selector(boxesFactory: PageNumber -> ColoredBoxWithNumber al1List, textPosition: TextPosition, specificTextColor: FsColor option) =
            Text(
                fun args info ->
                    let boxes = (args.PageNum) |> PageNumber |> boxesFactory
                    let exceptColors =
                        boxes.AsList
                        |> List.map (fun m -> m.Color)
                        |> FsColors.distinct
                        |> List.ofSeq

                    let textPositionPredicate = 
                        match textPosition with 
                        | TextPosition.InsideBox ->
                            Info.DenseBoundIsInsideOf(AreaGettingOptions.PageBox PageBoxKind.ActualBox) args info
                        | TextPosition.BothInsideAndOutsideBox -> true

                    let b =
                        textPositionPredicate
                        && (match specificTextColor with 
                            | Some color -> Info.FillColorIs color args info
                            | None -> true
                            )
                        && Info.FillColorIsNotOneOf(exceptColors) args info

                    if not textPositionPredicate then ()

                    match b with 
                    | true -> b
                    | false -> b

            )

        static member Flow_ReadTextsByOrder_PageModifier(boxesFactory: PageNumber -> ColoredBoxWithNumber al1List, textPosition: TextPosition, allowEmptyInsideBox, specificTextColor: FsColor option, filterInfos) =
            (fun args infos ->
                let textInfos = 
                    let infos = 
                        match filterInfos with 
                        | true -> 
                            let predicate =
                                ColoredBoxWithNumberAndTextFactory.Selector(boxesFactory, textPosition, specificTextColor)
                                |> Selector.toRenderInfoSelector args
                                |> RenderInfoSelector.toRenderInfoPredication

                            infos
                            |> List.ofSeq 
                            |> List.filter predicate

                        | false ->
                            infos
                            |> List.ofSeq

                    infos
                    |> List.choose(IIntegratedRenderInfo.asITextRenderInfo)
                    |> List.filter (fun m -> 
                        m.ConcatedText().Trim() <> ""
                    )


                let boxes = 
                    (args.PageNum) |> PageNumber |> boxesFactory
                    |> AtLeastOneList.sortBy_explictly<_, _>(fun m -> m.Number)

                let groupedInfos =
                    PageInfosSplitter.Groupby_CenterPointIsInside.Infos__GroupOrFilter_IntoOp(
                        (boxes.AsList |> List.map(fun m -> m.Box)),
                        textInfos,
                        (fun info ->
                            IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth info
                            |> Some
                            |> TargetPageBox.Create
                        )
                    )



                boxes.AsList
                |> List.mapi (fun i box ->
                    let (textInfos) =
                        groupedInfos.[i]
                        |> Option.map snd
                        |> Option.defaultValue []

                    let fonts = ColoredBoxFontAppearance.Read textInfos


                    let m = BoxWithText.Pick(box.Box, textInfos, allowEmptyInsideBox = allowEmptyInsideBox)
                    let m: ColoredBoxWithNumberAndText =
                        { ExtractorIndex = box.Number 
                          Text = 
                            match allowEmptyInsideBox with 
                            | true -> m.Text
                            | false ->
                                failwithf "Cannot Extract any text in ColoredBoxWithNumber %A" box

                          FontAppearance = fonts
                          PageNumber = PageNumber args.PageNum
                          Bound = { IndexedBound.Index = i; Bound = m.Box }.Serializable
                          Color = box.Color }

                    m
                )
                |> AtLeastOneList.Create
            )

        static member Flow_ReadTextsByOrder(boxesFactory: PageNumber -> ColoredBoxWithNumber al1List, textPosition: TextPosition, allowEmptyInsideBox, specificTextColor: FsColor option) =

            let flow =
                ModifyPage.Create(
                    "ReadTexts by Ordered ColoredBoxWithNumber",
                    PageSelector.All,
                    ColoredBoxWithNumberAndTextFactory.Selector(boxesFactory, textPosition, specificTextColor),
                    ColoredBoxWithNumberAndTextFactory.Flow_ReadTextsByOrder_PageModifier(boxesFactory, textPosition, allowEmptyInsideBox, specificTextColor, false)
                )

            flow

        static member Flow_Read(color, textPosition: TextPosition, specificTextColor: FsColor option) =
            
            let pos = textPosition
            Manipulate.Factory(fun flowModel document ->
                ColoredBoxWithNumber.Flow_Read(color, pos)
                <+>
                Manipulate.Func(fun colorBoxLists ->
                    let coloredBoxWithNumberLists =
                        colorBoxLists
                        |> List.map(fun (m: _OutsidesAndInsides<ColoredBoxWithNumber, ColoredBoxWithNumber>) ->
                            let coloredBoxWithNumbers = 
                                let insides =
                                    m.Insides

                                let outsides =
                                    m.Outsides

                                insides @ outsides
                                |> List.distinctBy(fun m -> m.Number)
                                |> List.sortBy(fun m -> m.Number)

                            coloredBoxWithNumbers
                        )

                    let coloredBoxWithTexts = 
                        let coloredBoxWithNumberLists =
                            coloredBoxWithNumberLists
                            |> List.map(AtLeastOneList.Create)

                        let factory (pageNum: PageNumber) =
                            coloredBoxWithNumberLists.[pageNum.Value-1]

                        ColoredBoxWithNumberAndTextFactory.Flow_ReadTextsByOrder(
                            factory,
                            pos,
                            allowEmptyInsideBox = true,
                            specificTextColor = specificTextColor
                        )
                
                    coloredBoxWithTexts
                    ||>> (fun m ->
                        match m with 
                        | [] -> failwithf "Cannot read any ColoredBoxWithNumber in %s" flowModel.PdfFile.Path
                        | v -> AtLeastOneList.Create v
                    )
                )
            )




        static member Read(pdfFile, color, textPosition: TextPosition, specificTextColor: FsColor option) =
            ColoredBoxWithNumberAndTextFactory.Flow_Read(color, textPosition, specificTextColor)
            |> Flow.Manipulate
            |> PdfRunner.OneFileFlow_UserState(pdfFile)
