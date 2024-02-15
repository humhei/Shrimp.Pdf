namespace Shrimp.Pdf.DSL

open Shrimp.Pdf.Colors
open Newtonsoft.Json
open iText.Kernel.Pdf.Xobject
open iText.IO.Image
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Refection
open Shrimp.FSharp.Plus.Text
open Shrimp.FSharp.Plus.Operators
open Shrimp.Pdf.RegisterableFonts

#nowarn "0104"
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf
open System.IO
open iText.Kernel.Pdf

open iText.Kernel.Geom

type StrokeWidthIncrenment =
    { Value: ``ufloat>0`` 
      LineJoinStyle: int }
with 
    static member Create(value, ?lineJoinStyle) =
        { Value = ``ufloat>0`` value 
          LineJoinStyle = defaultArg lineJoinStyle LineJoinStyle.MITER }

type FillOrStrokeModifingOptions =
    | Stroke = 0
    | Fill = 1
    | FillAndStroke = 2

type _SelectionModifierAddNewArguments<'userState> =
    { CurrentRenderInfo: IIntegratedRenderInfo  
      PageModifingArguments: PageModifingArguments<'userState> }

with 
    member x.Tag = x.CurrentRenderInfo.Tag

    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page

type _SelectionModifierFixmentArgumentsIM<'userState> =
    { CurrentRenderInfoIM: IIntegratedRenderInfoIM 
      PageModifingArguments: PageModifingArguments<'userState> }
with 
    member x.Tag = x.CurrentRenderInfoIM.TagIM

    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page


type _SelectionModifierFixmentArguments<'userState> =
    { CurrentRenderInfo: IIntegratedRenderInfo 
      PageModifingArguments: PageModifingArguments<'userState> }

with 
    member internal x.Tag = x.CurrentRenderInfo.Tag

    member x.PageNum = x.PageModifingArguments.PageNum

    member x.UserState = x.PageModifingArguments.UserState

    member x.Page = x.PageModifingArguments.Page




type PageInfosValidation = PageInfosValidation of (PageNumber -> seq<IIntegratedRenderInfo> -> unit)
with 
    member x.Value =
        let (PageInfosValidation value) = x
        value

[<RequireQualifiedAccess>]
module PageInfosValidation =
    let atLeastOne =
        fun (pageNumber) infos ->
            let length = (Seq.length infos)
            if length = 0 then failwith "Didn't selector any page infos"

        |> PageInfosValidation

    let ignore = 
        fun _ _ -> ()
        |> PageInfosValidation

type PageInfosValidationIM = PageInfosValidationIM of (PageNumber -> seq<IIntegratedRenderInfoIM> -> unit)
with 
    member x.Value =
        let (PageInfosValidationIM value) = x
        value

[<RequireQualifiedAccess>]
module PageInfosValidationIM =
    let atLeastOne =
        fun (pageNumber) infos ->
            let length = (Seq.length infos)
            if length = 0 then failwith "Didn't selector any page infos"

        |> PageInfosValidationIM

    let ignore = 
        fun _ _ -> ()
        |> PageInfosValidationIM

    let internal ofPageInfosValidation (pageInfosValidation: PageInfosValidation) =
        PageInfosValidationIM(fun pageNumber infos ->
            let infos = infos |> Seq.map(fun info -> info :?> IIntegratedRenderInfo)
            pageInfosValidation.Value pageNumber infos
            
        )



type Modifier<'userState> = _SelectionModifierFixmentArguments<'userState> -> ModifierPdfCanvasActions
type ModifierIM<'userState> = _SelectionModifierFixmentArgumentsIM<'userState> -> ModifierPdfCanvasActions

 [<RequireQualifiedAccess>]
 module private Modifier =
     let toIM (modifier: Modifier<_>): ModifierIM<_> =
         fun (args: _SelectionModifierFixmentArgumentsIM<'userState>) ->
             { CurrentRenderInfo = args.CurrentRenderInfoIM :?> IIntegratedRenderInfo
               PageModifingArguments = args.PageModifingArguments }
             |> modifier
 


type ShadableModifierIM<'userState> =
    { 
        SHShadingModifier: option<PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        ClippingPathShadingModifier: option<PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        CommonModifierIM: _SelectionModifierFixmentArgumentsIM<'userState> -> ModifierPdfCanvasActions
    }

type ShadableModifier<'userState> =
    { 
        SHShadingModifier: option<PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        ClippingPathShadingModifier: option<PageModifingArguments<'userState> -> IntegratedPathRenderInfo -> OperatorRange -> PdfCanvas -> unit>
        CommonModifier: _SelectionModifierFixmentArguments<'userState> -> ModifierPdfCanvasActions
    }
with 
    member x.ToIM() =
        { SHShadingModifier = x.SHShadingModifier 
          ClippingPathShadingModifier = x.ClippingPathShadingModifier 
          CommonModifierIM = Modifier.toIM x.CommonModifier }
        
 
[<RequireQualifiedAccess>]
type ModifierUnion<'userState> =
    | Modifier of Modifier<'userState>
    | ShadingModifier of ShadableModifier<'userState>
with 
    member x.CommonModifier =
        match x with 
        | Modifier vv -> vv
        | ShadingModifier vv -> vv.CommonModifier

[<RequireQualifiedAccess>]
type ModifierUnionIM<'userState> =
    | Modifier of ModifierIM<'userState>
    | ShadingModifier of ShadableModifierIM<'userState>
with 
    member x.CommonModifier =
        match x with 
        | Modifier vv -> vv
        | ShadingModifier vv -> vv.CommonModifierIM



[<RequireQualifiedAccess>]
module private ModifierUnion =
    let toIM (modifier: ModifierUnion<_>) =
        match modifier with 
        | ModifierUnion.Modifier vv -> Modifier.toIM vv |> ModifierUnionIM.Modifier
        | ModifierUnion.ShadingModifier vv -> vv.ToIM() |> ModifierUnionIM.ShadingModifier


[<RequireQualifiedAccess>]
module private Modifiers =
    let private toSelectionModifierCommon (pageModifingArguments: PageModifingArguments<_>) (modifiers: _ list) fArgs  =
        fun (args: _SelectionModifierFixmentArguments) ->
            let actions = 
                match args.CurrentRenderInfo with 
                | IIntegratedRenderInfoIM.Vector vectorRenderInfo ->
                    let args  = fArgs args

                    modifiers
                    |> List.mapi (fun i factory -> 
                        let actions: ModifierPdfCanvasActions = factory args
                        actions
                    )
                    |> ModifierPdfCanvasActions.ConcatOrKeep (vectorRenderInfo.Tag)
                
                | IIntegratedRenderInfoIM.Pixel pixelRenderInfo  ->
                    match modifiers with 
                    | [modifier] ->
                        let args  = fArgs args
                        modifier args
                    | _ -> failwithf "Multiple modifiers are not supported when render info tag is %A" args.CurrentRenderInfo.TagIM

            actions

    let toSelectionModifier (pageModifingArguments: PageModifingArguments<_>) (modifiers: Modifier<_> list) =
        toSelectionModifierCommon pageModifingArguments modifiers (fun args ->
            { PageModifingArguments = pageModifingArguments
              CurrentRenderInfo = args.CurrentRenderInfo :?> IIntegratedRenderInfo }
        )
      

    let toSelectionModifierIM (pageModifingArguments: PageModifingArguments<_>) (modifiers: ModifierIM<_> list) =
        toSelectionModifierCommon pageModifingArguments modifiers (fun args ->
            { PageModifingArguments = pageModifingArguments
              CurrentRenderInfoIM = args.CurrentRenderInfo }
        )
       
    let toSelectionModifierUnionIM (pageModifingArguments: PageModifingArguments<_>) (modifiers: ModifierUnionIM<_> list): ModifierUnion =
        let modifiers =
            List.choose2(fun m ->
                match m with 
                | ModifierUnionIM.Modifier v1 -> Choice1Of2 v1
                | ModifierUnionIM.ShadingModifier v2 -> Choice2Of2 v2
            ) modifiers

        match modifiers.Items1, modifiers.Items2 with 
        | [], [shadingModifier] ->
            let commonModifier = toSelectionModifierIM pageModifingArguments [shadingModifier.CommonModifierIM]
            ModifierUnion.ShadingModifier 
                {
                    SHShadingModifier = 
                        shadingModifier.SHShadingModifier |> Option.map (fun f -> f pageModifingArguments)
                    ClippingPathShadingModifier = 
                        shadingModifier.ClippingPathShadingModifier
                        |> Option.map (fun f -> f pageModifingArguments) 
                    CommonModifier = commonModifier
                }

        | _, _ :: _ -> failwithf "Multiple shadable modifiers is not supported"
        | _, [] -> 
            toSelectionModifierIM pageModifingArguments modifiers.Items1


            |> ModifierUnion.Modifier
open Constants.Operators



[<StructuredFormatDisplay("{LoggingText}")>]
type ColorMapping =
    { OriginColors: AlternativeFsColor al1List 
      NewColor: NullablePdfCanvasColor
      Tolerance: ValueEqualOptionsTolerance }
        
with 
    static member WhiteTo(newColor, ?tolerance) =
        { OriginColors = AtLeastOneList.Create AlternativeFsColor.Whites
          NewColor =  NullablePdfCanvasColor.OfPdfCanvasColor newColor
          Tolerance = defaultArg tolerance ValueEqualOptionsTolerance.DefaultValue }

    static member BlackTo(newColor, ?tolerance) =
        { OriginColors = AtLeastOneList.Create AlternativeFsColor.Blacks
          NewColor =  NullablePdfCanvasColor.OfPdfCanvasColor newColor
          Tolerance = defaultArg tolerance ValueEqualOptionsTolerance.DefaultValue }


    static member Create(originColors, newColor, ?tolerance) =
        { OriginColors = originColors
          NewColor = newColor
          Tolerance = defaultArg tolerance ValueEqualOptionsTolerance.DefaultValue }

    member x.LoggingText =
        let colors = x.OriginColors
        let loggingText =
            match colors with 
            | AtLeastOneList.One color -> color.LoggingText
            | AtLeastOneList.Many strokeColors -> 
                let colorsText =
                    strokeColors.AsList
                    |> List.map (fun m -> ValidFileName.Create m.LoggingText )
                    |> AtLeastOneList.Create
                    |> ConcactedText

                sprintf "multiple %d[%s]" 
                    strokeColors.Length 
                    (
                        colorsText.RangeText(50)
                    )
        sprintf "OriginColors: %s, NewColor:%O" loggingText x.NewColor
        

    override x.ToString() = x.LoggingText
        

type ColorMappings = ColorMappings of ColorMapping al1List
with 
    member x.Value =
        let (ColorMappings v1) = x
        v1

    member x.AsList =
        let (ColorMappings v1) = x
        v1.AsList

    member x.Add(newColorMapping) =
        let (ColorMappings colorMapping) = x
        colorMapping.Add [newColorMapping]
        |> ColorMappings

    static member Create(colorMapping) =
        colorMapping
        |> AtLeastOneList.singleton 
        |> ColorMappings

    static member Concat(colorMappingLists: ColorMappings al1List) =
        colorMappingLists
        |> AtLeastOneList.collect(fun (ColorMappings colorMappings) -> colorMappings)
        |> ColorMappings

    static member WhiteTo(newColor) =
        ColorMapping.WhiteTo(newColor)
        |> List.singleton
        |> AtLeastOneList.Create
        |> ColorMappings

    static member BlackTo(newColor) =
        ColorMapping.BlackTo(newColor)
        |> List.singleton
        |> AtLeastOneList.Create
        |> ColorMappings

    member x.AsPicker() =
        let (ColorMappings colorMappings) = x
        fun (fsColor: FsColor) ->
            colorMappings.AsList
            |> List.tryPick(fun colorMapping ->
                let colors = List.map FsColor.OfAlternativeFsColor colorMapping.OriginColors.AsList
                match FsColors.containsWith (ValueEqualOptions.RoundedValue colorMapping.Tolerance) fsColor colors with 
                | true -> Some colorMapping.NewColor
                    
                | false -> None
            )


    member x.LoggingText =
        x.AsList
        |> List.map(fun m -> m.LoggingText)
        |> String.concat "\n"


[<RequireQualifiedAccess>]
type CloseStyle = 
    | Close
    | Open of PdfCanvasColor
    | Keep 
    | KeepOperationAndChangeColor of PdfCanvasColor

type ColorStyle [<JsonConstructor>] private (v) =
    inherit POCOBaseV<NullablePdfCanvasColor option * CloseOperator option * bool option * float32 option>(v) 

        member x.NullablePdfCanvasColor   = x.VV.Item4_1()

        member x.CloseOperator            = x.VV.Item4_2()

        member x.Overprint                = x.VV.Item4_3()

        member x.Opacity                = x.VV.Item4_4()

        new(?color, ?closeOperator, ?overprint, ?opacity) =
            DefaultArgs.CheckAllInputsAreNotEmpty(color, closeOperator, overprint, opacity)
            new ColorStyle((color, closeOperator, overprint, opacity))

    static member DefaultValue = ColorStyle()

        



type StrokeStyle [<JsonConstructor>] private  (v) =
    inherit POCOBaseV<ColorStyle option * ``ufloat>0`` option * int option * DashPattern option>(v)

    member x.ColorStyle = x.VV.Item4_1()

    member x.Width = x.VV.Item4_2() 

    member x.LineJoinStyle = x.VV.Item4_3()

    member x.DashPattern = x.VV.Item4_4()

    member x.CloseOperator =
        match x.ColorStyle with 
        | None -> None
        | Some colorStyle -> colorStyle.CloseOperator
        |> Option.defaultValue CloseOperator.Keep


    member x.NullablePdfCanvasColor =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.NullablePdfCanvasColor
        | None -> None

    member x.Overprint =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.Overprint
        | None -> None

    member x.Opacity =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.Opacity
        | None -> None

    member x.AsModifier(args: _SelectionModifierFixmentArguments<_>) =
        let pdfCanvasActions = 
            [
                match x.NullablePdfCanvasColor with 
                | None -> ()
                | Some canvasColor -> PdfCanvas.SetStrokeColor(canvasColor)

                match x.Width with 
                | None -> ()
                | Some width -> PdfCanvas.setLineWidth width.Value

                match x.LineJoinStyle with 
                | None -> ()
                | Some lineJoinStyle -> PdfCanvas.setLineJoinStyle lineJoinStyle

                match x.Overprint, x.Opacity with 
                | None, None -> ()
                | _ ->
                    
                    let originExtGState = IAbstractRenderInfo.getExtGState args.CurrentRenderInfo
                    let setOverprint(extGState: FsExtGState) = 
                        match x.Overprint with 
                        | Some overprint -> extGState.SetStrokeIsOverprint overprint
                        | None -> extGState

                    let setOpacity(extGState: FsExtGState) = 
                        match x.Opacity with 
                        | Some opacity -> extGState.SetStrokeOpacity(opacity)
                        | None -> extGState

                    originExtGState
                    |> setOverprint
                    |> setOpacity
                    |> PdfCanvas.setExtGState

                match x.DashPattern with 
                | None -> ()
                | Some dashPattern ->
                    PdfCanvas.setDashpattern dashPattern

            ]
        
        { Actions = pdfCanvasActions
          SuffixActions = []
          Close = CloseOperatorUnion.Create(args.Tag, stroke = x.CloseOperator) }


    new (?targetColorStyle: ColorStyle, ?width: ``ufloat>0``, ?lineJoinStyle, ?dashPattern) =
        DefaultArgs.CheckAllInputsAreNotEmpty(targetColorStyle, width, lineJoinStyle, dashPattern)
        new StrokeStyle((targetColorStyle, width, lineJoinStyle, dashPattern))

    new (args: PdfCanvasAddLineArguments) =
        new StrokeStyle(
            ColorStyle(NullablePdfCanvasColor.OfPdfCanvasColor args.StrokeColor),
            width = (``ufloat>0`` args.LineWidth),
            dashPattern = args.DashPattern
        )

    static member ColorIs(color) =
        StrokeStyle(
            ColorStyle(NullablePdfCanvasColor.OfPdfCanvasColor color)
        )

    static member OpacityIs(opacity: float32) =
        StrokeStyle(
            ColorStyle(opacity = opacity)
        )


type FillStyle [<JsonConstructor>] (?v) =
    inherit POCOBaseV<ColorStyle option>(v)

    member x.ColorStyle = x.VV.Value

    member x.NullablePdfCanvasColor =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.NullablePdfCanvasColor
        | None -> None

    member x.Overprint =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.Overprint
        | None -> None

    member x.Opacity =
        match x.ColorStyle with
        | Some colorStyle -> colorStyle.Opacity
        | None -> None


    member x.CloseOperator =
        match x.ColorStyle with 
        | None -> None
        | Some colorStyle -> colorStyle.CloseOperator
        |> Option.defaultValue CloseOperator.Keep


    member x.AsModifier(args: _SelectionModifierFixmentArguments<_>) =
        let pdfCanvasActions = 
            [
                match x.NullablePdfCanvasColor with 
                | None -> ()
                | Some canvasColor -> PdfCanvas.SetFillColor(canvasColor)

                match x.Overprint, x.Opacity with 
                | None, None -> ()
                | _ ->
                    
                    let originExtGState = IAbstractRenderInfo.getExtGState args.CurrentRenderInfo
                    let newExtGState =
                        let fill = originExtGState.Fill
                        let newFill = 
                            { fill with 
                                Opacity = 
                                    match x.Opacity with 
                                    | None -> fill.Opacity
                                    | Some opacity -> opacity

                                IsOverprint =
                                    match x.Overprint with 
                                    | Some overprint -> overprint
                                    | None -> fill.IsOverprint
                            }
                        { originExtGState with 
                            Fill = newFill        
                        }
                    
                    PdfCanvas.setExtGState newExtGState

            ]
 
        { Actions = pdfCanvasActions
          SuffixActions = []
          Close = CloseOperatorUnion.Create(args.Tag, fill = x.CloseOperator) }

    static member ColorIs(color) =
        FillStyle(
            ColorStyle(NullablePdfCanvasColor.OfPdfCanvasColor color)
        )

    static member OpacityIs(opacity) =
        FillStyle(
            ColorStyle(opacity = opacity)
        )



type ResizingStyle [<JsonConstructor>] (v) =
    inherit POCOBaseV<FsSize>(v)

    member x.Size = x.VV.Value

    member x.AsPdfActions(args: _SelectionModifierFixmentArguments<_>) =
        let originSize = 
            IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth args.CurrentRenderInfo
            |> FsSize.ofRectangle

        let scaleX = x.Size.Width / originSize.Width

        let scaleY = x.Size.Height / originSize.Height

        let width_difference = x.Size.Width - originSize.Width

        let height_difference = x.Size.Height - originSize.Height

        let originCtm = 
            args.CurrentRenderInfo.Value.GetGraphicsState().GetCtm()
            |> AffineTransformRecord.ofMatrix



        let transform = 
            { AffineTransformRecord.DefaultValue with 
                ScaleX = scaleX 
                ScaleY = scaleY
                TranslateX = -width_difference / 2. / originCtm.ScaleX
                TranslateY = -height_difference / 2. / originCtm.ScaleY
                
            }
        [
            PdfCanvas.concatMatrixByTransformRecord transform
        ]

type FlipStyle [<JsonConstructor>] private (v) =
    inherit POCOBaseV<Flip * PageBoxKind option >(v)

    member x.FlippingWay = x.VV.Item2_1()

    member x.PageBoxKind = x.VV.Item2_2()


    member x.AsPdfActions(args: _SelectionModifierFixmentArguments<_>) =
        let originSize = 
            IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth args.CurrentRenderInfo
        
        let originCtm = 
            args.CurrentRenderInfo.Value.GetGraphicsState().GetCtm()
            |> AffineTransformRecord.ofMatrix

        let transform =
            match x.PageBoxKind with 
            | None ->
                let baseTransform = AffineTransformRecord.GetFlipInstance x.FlippingWay
                { baseTransform with 
                    TranslateX = 
                        match x.FlippingWay with 
                        | Flip.HFlip -> 
                            let offsetByCtm = ((originSize.GetRightF() - originCtm.TranslateX) * 2.)
                            let offsetByCenter = (-originSize.GetWidthF() + offsetByCtm)
                            offsetByCenter / originCtm.ScaleX
                        | Flip.VFlip -> 0.

                    TranslateY = 
                        match x.FlippingWay with 
                        | Flip.VFlip -> 
                            let offsetByCtm = ((originCtm.TranslateY - originSize.GetBottomF()) * 2.)
                            let offsetByCenter = (-originSize.GetHeightF() - offsetByCtm)
                            offsetByCenter / originCtm.ScaleY
                        | Flip.HFlip -> 0.
                }

            | Some pageBoxKind ->
                let pageBox = args.Page.GetPageBox(pageBoxKind)
                let baseTransform = AffineTransformRecord.GetFlipInstance x.FlippingWay
                let translate = 
                    { baseTransform with 
                        TranslateX = 
                            match x.FlippingWay with 
                            | Flip.HFlip -> 
                                let offsetByCtm = ((originSize.GetRightF() - originCtm.TranslateX) * 2.)
                                let offsetByCenter = (-originSize.GetWidthF() + offsetByCtm)
                                let middleSpace = (originSize.GetXF() - pageBox.GetXCenterF()) * 2.
                                
                                (offsetByCenter - middleSpace - originSize.GetWidthF()) / originCtm.ScaleX
                            | Flip.VFlip -> 0.

                        TranslateY = 
                            match x.FlippingWay with 
                            | Flip.VFlip -> 
                                let offsetByCtm = ((originCtm.TranslateY - originSize.GetBottomF()) * 2.)
                                let offsetByCenter = (-originSize.GetHeightF() - offsetByCtm)
                                let middleSpace = (originSize.GetYF() - pageBox.GetYCenterF()) * 2.
                                (offsetByCenter - middleSpace - originSize.GetHeightF()) / originCtm.ScaleY

                            | Flip.HFlip -> 0.
                    }

                translate

        //let transform = 
        //    match x.PageBoxKind with 
        //    | None ->
        //        let baseTransform = AffineTransformRecord.GetFlipInstance x.FlippingWay
        //        let translate = 
        //            { AffineTransformRecord.DefaultValue with 
        //                TranslateX = 
        //                    match x.FlippingWay with 
        //                    | Flip.HFlip -> -originSize.GetWidthF() / originCtm.ScaleX
        //                    | Flip.VFlip -> 0.

        //                TranslateY = 
        //                    match x.FlippingWay with 
        //                    | Flip.VFlip -> -originSize.GetHeightF() / originCtm.ScaleY
        //                    | Flip.HFlip -> 0.
        //            }

        //        originCtm.Concatenate(baseTransform)

        //        //baseTransform.Concatenate translate

        //    | Some pageBoxKind ->
        //        let pageBox = args.Page.GetPageBox(pageBoxKind)
        //        let baseTransform = AffineTransformRecord.GetFlipInstance x.FlippingWay
        //        let translate = 
        //            { AffineTransformRecord.DefaultValue with 
        //                TranslateX = 
        //                    match x.FlippingWay with 
        //                    | Flip.HFlip -> 
        //                        let middleSpace = (originSize.GetXF() - pageBox.GetXCenterF()) * 2.
                                
        //                        ((-originSize.GetWidthF() * 2.) - middleSpace) 
        //                    | Flip.VFlip -> 0.

        //                TranslateY = 
        //                    match x.FlippingWay with 
        //                    | Flip.VFlip -> -originSize.GetHeightF() 
        //                    | Flip.HFlip -> 0.
        //            }

        //        failwith ""


        [
            PdfCanvas.concatMatrixByTransformRecord transform
        ]


    new (?flip, ?pageBoxKind) =
        FlipStyle((defaultArg flip Flip.HFlip, pageBoxKind))

[<RequireQualifiedAccess>]
type TransformStyle =   
    | Resize of ResizingStyle
    | Flip of FlipStyle
with 
    member x.AsPdfActions(args) =
        match x with 
        | Resize v -> v.AsPdfActions(args)
        | Flip v -> v.AsPdfActions(args)


type BackgroundOrForegroundFile =
    { File: BackgroundFile
      BackgroundOrForeground: BackgroundOrForeground }



type VectorStyle [<JsonConstructor>] private (v) =
    inherit POCOBaseV<FillStyle option * StrokeStyle option * TransformStyle option * bool>(v)

    member x.FillStyle = x.VV.Item4_1()

    member x.StrokeStyle = x.VV.Item4_2()
    
    member x.ResizingStyle = x.VV.Item4_3() 

    member x.AsCopy = x.VV.Item4_4() 

    member x.FillOverprint = 
        match x.FillStyle with 
        | Some style -> style.Overprint
        | None -> None

    //static member MethodConversion(?fillStyle: FillStyle, ?strokeStyle: StrokeStyle): MethodLiteralConversion<_> =
    //    let name = nameof(VectorStyle)
    //    let alternativeFsColorText = 
    //        match fillColor with 
    //        | Some (color) -> color.LoggingText_Raw
    //        | None -> ""

    //    let inboxAreaText =
    //        match leftBottomBasedInboxArea with 
    //        | None -> ""
    //        | Some inboxArea -> inboxArea.LoggingText


    //    {
    //        MethodLiteral = 
    //            { Name = name
    //              Parameters = 
    //                [
    //                    nameof(fontName)    ==> defaultArg fontName ""
    //                    nameof(fontSize)    ==> defaultArg fontSize 0.
    //                    nameof(fillColor)   ==> alternativeFsColorText
    //                    nameof(leftBottomBasedInboxArea)   ==> inboxAreaText
    //                    nameof(textPattern) ==> defaultArg textPattern ""

    //                ]
    //                |> Observations.Create
    //              }

    //        OfMethodLiteral = (fun (method: MethodLiteral) ->
    //            match method.Name with 
    //            | EqualTo name ->
    //                let fontName = 
    //                    method.Parameters.[nameof(fontName)].Value.Text
    //                    |> String.asOption

    //                let fontSize= 
    //                    method.Parameters.[nameof(fontSize)].Value.Text
    //                    |> System.Double.Parse
    //                    |> function 
    //                        | 0. -> None
    //                        | fontSize -> Some fontSize

    //                let fillColor = 
    //                    method.Parameters.[nameof(fillColor)].Value.Text
    //                    |> String.asOption
    //                    |> Option.map(AlternativeFsColor.OfLoggingText_Raw >> FsColor.OfAlternativeFsColor)

    //                let inboxArea = 
    //                    method.Parameters.[nameof(leftBottomBasedInboxArea)].Value.Text
    //                    |> String.asOption
    //                    |> Option.map(MMRectangle.Parse)
    //                    |> Option.map(fun rect ->
    //                        Info_BoundIs_Args(RelativePosition.Inbox, AreaGettingOptions.FsSpecfic rect.AsFsRectangle)
    //                    )

    //                let textPattern =
    //                    match method.Parameters.[nameof(textPattern)].Value.Text.Trim() with 
    //                    | "" -> None
    //                    | text ->
    //                        TextSelectorOrTransformExpr.Parse text
    //                        |> Some

    //                FontAndSizeQuery(
    //                    ?fontName = fontName,
    //                    ?fontSize = fontSize,
    //                    ?fillColor = fillColor,
    //                    ?info_BoundIs_Args = inboxArea,
    //                    ?textPattern = textPattern
    //                )
    //                |> Some
                    
    //            | _ -> None
    //        )
    //    }

    member x.StrokeOverprint = 
        match x.StrokeStyle with 
        | Some style -> style.Overprint
        | None -> None

    member x.AsModifier(args: _SelectionModifierFixmentArguments<_>) =
        let modiferActions = 
            let modifierActions = 
                match x.FillStyle, x.StrokeStyle with 
                | Some fillStyle, Some strokeStyle ->   
                    let concated = 
                        [
                            fillStyle.AsModifier(args)
                            strokeStyle.AsModifier(args)
                        ]
                        |> ModifierPdfCanvasActions.ConcatOrKeep(args.Tag)

                    match x.FillOverprint, x.StrokeOverprint with 
                    | None, None 
                    | Some _, None
                    | None, Some _ -> concated
                    | Some fillOverprint, Some strokeOverprint ->
                        let originExtGState = IAbstractRenderInfo.getExtGState args.CurrentRenderInfo
                        let newExtGState = 
                            originExtGState
                                .SetStrokeIsOverprint(strokeOverprint)
                                .SetFillIsOverprint(fillOverprint)

                        { concated with 
                            Actions =
                                concated.Actions
                                @ 
                                [
                                    PdfCanvas.setExtGState newExtGState
                                ]
                        }

                | Some style, None -> style.AsModifier(args)
                | None, Some style -> style.AsModifier(args)
                | None, None -> ModifierPdfCanvasActions.Keep(args.Tag)
        
            modifierActions.AddActions[
                match x.ResizingStyle with 
                | Some resizeStyle ->
                    yield! resizeStyle.AsPdfActions(args)
                | None -> ()
            ]

        match x.AsCopy with 
        | false -> modiferActions
        | true ->
            match args.CurrentRenderInfo with 
            | IIntegratedRenderInfo.Text _ -> failwithf "Cloning text object feature currently is not supported"
            | IIntegratedRenderInfo.Path pathInfo ->
                { Actions = 
                    [
                        PdfCanvas.closePathByOperation (pathInfo.PathRenderInfo.GetOperation()) (pathInfo.PathRenderInfo.GetRule())
                        yield! 
                            pathInfo.AccumulatedPathOperatorRanges
                            |> Seq.map(fun operatorRange -> 
                                let r = PdfCanvas.writeOperatorRange operatorRange
                                r
                            )
                        yield! modiferActions.Actions
                    ] 
                  Close = modiferActions.Close
                  SuffixActions = modiferActions.SuffixActions
                }



    new (?fill, ?stroke, ?transformStyle, ?asCopy) =
        DefaultArgs.CheckAllInputsAreNotEmpty(fill, stroke, transformStyle, asCopy)
    
        new VectorStyle((fill, stroke, transformStyle, defaultArg asCopy false))

    static member ColorIs(?fillColor, ?strokeColor) =
        DefaultArgs.CheckAllInputsAreNotEmpty(fillColor, strokeColor)
        let fill =
            match fillColor with 
            | Some fillColor -> Some (FillStyle.ColorIs(fillColor))
            | None -> None

        let stroke =
            match strokeColor with 
            | Some strokeColor -> Some (StrokeStyle.ColorIs(strokeColor))
            | None -> None

        VectorStyle(?fill = fill, ?stroke = stroke)

    static member OpacityIs(?fillOpacity, ?strokeOpacity) =
        DefaultArgs.CheckAllInputsAreNotEmpty(fillOpacity, strokeOpacity)
        
        let fill =
            match fillOpacity with 
            | Some fillOpacity -> Some (FillStyle.OpacityIs(fillOpacity))
            | None -> None

        let stroke =
            match strokeOpacity with 
            | Some strokeOpacity -> 
                Some (StrokeStyle.OpacityIs(strokeOpacity))
                //Some (
                //    StrokeStyle(
                //        ColorStyle(opacity = strokeOpacity)
                //        //(strokeOpacity))
                //    )
                //)

            | None -> None

        VectorStyle(?fill = fill, ?stroke = stroke)

[<RequireQualifiedAccess>]
type ClippingCondition =
    | ClipIfPathCountSmallerOrEqualThan of int
    | Always
with 
    member x.IsClippable(infos: Path list) =
        match x with 
        | ClippingCondition.Always -> true
        | ClippingCondition.ClipIfPathCountSmallerOrEqualThan count ->
            let infosLength =
                infos
                |> List.sumBy(fun m -> 
                    let mutable count = 0
                    for subPath in m.GetSubpaths() do
                        match subPath.GetSegments().Count with 
                        | 0 -> ()
                        | _ -> count <- count + 1

                    count
                )

            //let infosLength = infos.Length

            infosLength <= count

[<RequireQualifiedAccess>]
type private CompoundCreatingOptions =
    | CompoundPath 
    | ClippingPathAndCancel   of clipIfPathCountSmallerThan: ClippingCondition
    | ClippingPathAndKeep     of clipIfPathCountSmallerThan: ClippingCondition


type NewFontAndSize [<JsonConstructor>] (?font: FsPdfFontFactory, ?fontSize: float, ?alignment) = 
    inherit POCOBase<FsPdfFontFactory option * float option * XEffort option>(font, fontSize, alignment)
    let __checkArgsValid =
        match font, fontSize with 
        | None, None -> failwith "Cannot Create NewFontAndSize, both font and fontSize are empty"
        | _ -> ()

    [<JsonProperty>]
    member x.Font = font

    [<JsonProperty>]
    member x.FontSize = fontSize

    [<JsonProperty>]
    member x.Alignment = alignment

    member x.LoggingText =
        let font =
            match font with 
            | Some font -> font.LoggingText |> Some 
            | None -> None

        let fontSize = 
            match fontSize with 
            | Some fontSize -> fontSize.ToString() |> Some 
            | None -> None

        let alignment = 
            match alignment with 
            | Some alignment -> alignment.ToString() |> Some 
            | None -> None

        [ font 
          fontSize
          alignment ]
        |> List.choose id
        |> String.concat " "



[<RequireQualifiedAccess>]
type PasteObjectSize =
    | SameToBackgroundSize
    | BySelection of Margin

type Modifier =
    static member ChangeStyle(fStyle: _ -> VectorStyle) = 
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            (fStyle args.CurrentRenderInfo).AsModifier(args)

    static member AddBackgroundOrForeground(backgroundFile: BackgroundOrForegroundFile, pasteObjectSize: PasteObjectSize) = 
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let xobject =
                let backgroundFile = backgroundFile.File
                (args.Page.GetDocument() :?> PdfDocumentWithCachedResources)
                    .GetOrCreateXObject(backgroundFile.ClearedPdfFile)

            let pdfActions = 
                let currentTransform = 
                    args.CurrentRenderInfo.Value.GetGraphicsState().GetCtm() 
                    |> AffineTransform.ofMatrix

                let newTransform = 
                    let inversed = 
                        currentTransform |> AffineTransform.inverse

                    let bound =     
                        IAbstractRenderInfo.getBound
                            BoundGettingStrokeOptions.WithoutStrokeWidth
                            args.CurrentRenderInfo

                    let backgroundSize = backgroundFile.File |> BackgroundFile.getSize

                    let pasteObjectSize =
                        match pasteObjectSize with 
                        | PasteObjectSize.SameToBackgroundSize -> backgroundSize
                        | PasteObjectSize.BySelection margin ->
                            bound
                            |> FsSize.ofRectangle
                            |> FsSize.applyMargin margin

                    let translate =
                        { AffineTransformRecord.DefaultValue with 
                            TranslateX = bound.GetXF() - ((pasteObjectSize.Width - bound.GetWidthF()) / 2.0)
                            TranslateY = bound.GetYF() - ((pasteObjectSize.Height - bound.GetHeightF()) / 2.0)
                        }

                    let scale =
                        { AffineTransformRecord.DefaultValue with 
                            ScaleX = pasteObjectSize.Width   / backgroundSize.Width
                            ScaleY = pasteObjectSize.Height  / backgroundSize.Height
                        }

                    inversed.Concatenate(AffineTransformRecord.toAffineTransform translate)
                    inversed.Concatenate(AffineTransformRecord.toAffineTransform scale)
                    AffineTransformRecord.ofAffineTransform inversed

                [ PdfCanvas.saveState
                  PdfCanvas.addXObject xobject newTransform
                  PdfCanvas.restoreState ]

            match backgroundFile.BackgroundOrForeground with 
            | BackgroundOrForeground.Background ->
                ModifierPdfCanvasActions.CreateActions
                    (args.Tag)
                    pdfActions

            | BackgroundOrForeground.Foreground ->
                ModifierPdfCanvasActions.CreateSuffix
                    (args.Tag)
                    pdfActions


    static member AddBackground(backgroundFile: BackgroundFile, pasteObjectSize: PasteObjectSize) =
        Modifier.AddBackgroundOrForeground(
            { File = backgroundFile 
              BackgroundOrForeground = BackgroundOrForeground.Background },
            pasteObjectSize
        )

    static member DecodeText(): Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            match args.Tag with 
            | IntegratedRenderInfoTag.Path ->
                ModifierPdfCanvasActions.Keep(args.Tag)

            | IntegratedRenderInfoTag.Text ->
                let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                let text = ITextRenderInfo.getPdfConcatedText info
                ModifierPdfCanvasActions.CreateCloseOperator(args.Tag, text = text)

    static member SetFontAndSize(font, size: float, ?alignment: XEffort) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let alignment = defaultArg alignment XEffort.Left
            match args.Tag with 
            | IntegratedRenderInfoTag.Path ->
                ModifierPdfCanvasActions.Keep(args.Tag)

            | IntegratedRenderInfoTag.Text ->
                let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                let transformedFontSize = ITextRenderInfo.toTransformedFontSize size info 
                let text = ITextRenderInfo.getPdfConcatedText info
                let difference() =
                    let originWidth = ITextRenderInfo.getWidth info
                    let newWidth =
                        text.SplitByLine()
                        |> List.map(fun text ->
                            let widthUnits = 
                                let text = text.ConcatedText()
                                PdfFont.calcLineWidthUnits text font
                            List.max widthUnits * size
                        )
                        |> List.max

                    newWidth - originWidth

                let originTransform() = 
                    info.TextRenderInfo.GetTextMatrix()
                    |> AffineTransform.ofMatrix

                { Actions =
                    [
                        match alignment with 
                        | XEffort.Left -> ()
                        | XEffort.Middle ->
                            let offset = -(difference() / 2.) * transformedFontSize / size
                            let transform = originTransform()
                            transform.Translate(offset, 0.)
                            PdfCanvas.setTextMatrixByTransform(AffineTransformRecord.ofAffineTransform transform)

                        | XEffort.Right ->
                            let offset = difference()
                            let transform = 
                                { AffineTransformRecord.DefaultValue with 
                                    TranslateX = offset }
                            PdfCanvas.setTextMatrixByTransform(transform)



                        PdfCanvas.setFontAndSize(font, float32 transformedFontSize)
                        //PdfCanvas.writeOperatorRange args.Close
                    ]
                    
                  Close = 
                    CloseOperatorUnion.Text(
                        { Fill = CloseOperator.Keep 
                          Stroke = CloseOperator.Keep
                          Text = Some text }
                    )
                  SuffixActions = []
                }


    static member SetFontAndSize(font, size: float, ?alignment) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
            let font = doc.GetOrCreatePdfFont(font)
            Modifier.SetFontAndSize(font, size, ?alignment = alignment) args

    static member NewFontAndSize(fontAndSize: NewFontAndSize) =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let font =
                let font = fontAndSize.Font 
                match font with 
                | Some font -> 
                    let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                    doc.GetOrCreatePdfFont(font)

                | None ->
                    let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                    info.TextRenderInfo.GetFont()

            let size =
                let fontSize = fontAndSize.FontSize
                match fontSize with 
                | Some fontSize -> fontSize
                | None ->
                    let info = args.CurrentRenderInfo :?> IntegratedTextRenderInfo
                    ITextRenderInfo.getActualFontSize info

            Modifier.SetFontAndSize(font, size, ?alignment = fontAndSize.Alignment) args

    static member CancelFillAndStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                fill = CloseOperator.Close,
                stroke = CloseOperator.Close
            )
         
    

    static member OpenFill(?fillColor) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            { Actions = 
                [
                    match fillColor with 
                    | Some (fillColor: PdfCanvasColor) -> 
                        PdfCanvas.SetFillColor (NullablePdfCanvasColor.OfPdfCanvasColor fillColor)
                    | None -> ()
                ]
              SuffixActions = []
              Close = CloseOperatorUnion.Create(args.Tag, fill = CloseOperator.Open)}


    static member OpenStroke(?strokeColor) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            { Actions = 
                [
                    match strokeColor with 
                    | Some (fillColor: PdfCanvasColor) -> 
                        PdfCanvas.SetStrokeColor (NullablePdfCanvasColor.OfPdfCanvasColor fillColor)
                    | None -> ()
                ]
              SuffixActions = []
              Close = CloseOperatorUnion.Create(args.Tag, stroke = CloseOperator.Open)}
          
        

    static member CancelStroke() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                stroke = CloseOperator.Close
            )
       


    static member CancelFill() : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            ModifierPdfCanvasActions.CreateCloseOperator(
                tag = args.Tag,
                fill = CloseOperator.Close
            )
        


    static member internal ReplaceColorCommon(picker: (_ -> FsColor -> Choice<Color, NullablePdfCanvasColor> option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let fillOrStrokeModifyingOptions = defaultArg fillOrStrokeModifyingOptions FillOrStrokeModifingOptions.FillAndStroke

        fun (args: _SelectionModifierFixmentArguments<'userState>)  ->
            let getOrCreateColor (color: Choice<Color, NullablePdfCanvasColor>) =
                match color with 
                | Choice.Choice1Of2 color -> Some color
                | Choice.Choice2Of2 color ->
                    let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                    match color with 
                    | NullablePdfCanvasColor.Non -> None
                    | NullablePdfCanvasColor.PdfCanvasColor color ->
                        doc.GetOrCreateColor(color)
                        |> Some

            let cancelOperation1 = 
                if IAbstractRenderInfo.hasFill args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker args (args.CurrentRenderInfo.Value.GetFillColor() |> FsColor.OfItextColor) with
                    | FillOrStrokeModifingOptions.Stroke, _ 
                    | _, None -> ModifierPdfCanvasActions.Keep(args.Tag)
                    | FillOrStrokeModifingOptions.Fill, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        match getOrCreateColor newColor with 
                        | Some newColor ->
                            [PdfCanvas.setFillColor(newColor)]
                            |> ModifierPdfCanvasActions.CreateActions args.Tag
                        | None ->
                            ModifierPdfCanvasActions.CreateCloseOperator(args.Tag, fill = CloseOperator.Close)
                else ModifierPdfCanvasActions.Keep(args.Tag)
                         
            let cancelOperation2 = 

                if IAbstractRenderInfo.hasStroke args.CurrentRenderInfo 
                then 
                    match fillOrStrokeModifyingOptions, picker args (args.CurrentRenderInfo.Value.GetStrokeColor() |> FsColor.OfItextColor) with
                    | FillOrStrokeModifingOptions.Fill, _ 
                    | _, None -> ModifierPdfCanvasActions.Keep(args.Tag)
                    | FillOrStrokeModifingOptions.Stroke, Some (newColor) 
                    | FillOrStrokeModifingOptions.FillAndStroke, Some (newColor) ->
                        match getOrCreateColor newColor with 
                        | Some newColor ->
                            [PdfCanvas.setStrokeColor(newColor)]
                            |> ModifierPdfCanvasActions.CreateActions args.Tag

                        | None -> 
                            ModifierPdfCanvasActions.CreateCloseOperator(args.Tag, stroke = CloseOperator.Close)
                else ModifierPdfCanvasActions.Keep(args.Tag)

            ModifierPdfCanvasActions.ConcatOrKeep args.Tag [cancelOperation1; cancelOperation2]

    static member private ReplaceColorEx(picker: (_ -> FsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon((fun args color -> picker args color |> Option.map Choice1Of2), ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)

    static member ReplaceColorNullable(picker: (_ -> FsColor -> NullablePdfCanvasColor option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon(
            picker = (fun info color -> picker info color |> Option.map Choice2Of2),
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions
        )


    static member ReplaceColor(picker: (FsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColorCommon(
            picker = (fun info color -> picker color |> Option.map Choice1Of2),
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions
        )

    static member ReplaceColors(colorMappings: ColorMappings, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
            let picker = 
                colorMappings.AsPicker() >> Option.map(fun color ->
                color
            )

            Modifier.ReplaceColorNullable((fun args color -> picker color), ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)
            

    static member ReplaceAlternativeColor(picker: (AlternativeFsColor -> Color option), ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) =
        let picker (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> picker color
            | None -> None

        Modifier.ReplaceColor(picker, ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions)

    static member ReplaceColor(colorKeyValuePairs: list<Color * Color>, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        let originColors = 
            colorKeyValuePairs 
            |> List.map fst
            |> List.map (FsColor.OfItextColor)

        let newColors = 
            colorKeyValuePairs 
            |> List.map snd


        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker =
                fun color ->
                    let color = color
                    match FsColors.tryFindIndex color originColors with 
                    | Some index -> Some newColors.[index]
                    | None -> None
        )

    static member InvertColors(?fillOrStrokeModifyingOptions) =
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker = 
                fun color ->
                    match color.AlterColor with 
                    | None -> None
                    | Some color ->
                        (color)
                        |> FsValueColor.Invert
                        |> FsValueColor.ToItextColor
                        |> Some
        )
    
    static member SetColor(color: Color, ?fillOrStrokeModifyingOptions: FillOrStrokeModifingOptions) : Modifier<'userState> =
        Modifier.ReplaceColor(
            ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
            picker = fun _ -> Some color
        )

    static member SetFillColor(color: Color) : Modifier<'userState> =
        Modifier.ReplaceColor(
            fillOrStrokeModifyingOptions = FillOrStrokeModifingOptions.Fill,
            picker = fun _ -> Some color
        )

    static member SetStrokeColor(color: Color) : Modifier<'userState> =
        Modifier.ReplaceColor(
            fillOrStrokeModifyingOptions = FillOrStrokeModifingOptions.Stroke,
            picker = fun _ -> Some color
        )



    static member AddRectangleToBound(mapping) : Modifier<'userState> =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let border = IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth args.CurrentRenderInfo

            ModifierPdfCanvasActions.CreateSuffix 
                args.Tag 
                [
                    PdfCanvas.addRectangle border mapping
                ]
              
    static member ChangeStrokeStyle(targetStyle: StrokeStyle) =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            targetStyle.AsModifier(args)
          

    static member ExpandStrokeWidth(targetColor: PdfCanvasColor, width: ``ufloat>0``, ?lineJoinStyle, ?overprint) =
        let width = width.Value
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            let info =  args.CurrentRenderInfo
            let originExtGSState = args.CurrentRenderInfo.Value.GetGraphicsState() |> CanvasGraphicsState.getExtGState
            let newExtGSState = originExtGSState.SetStrokeIsOverprint(true)
            let setStroke(width) =
                [
                    PdfCanvas.setLineWidth width
                    PdfCanvas.SetStrokeColor (NullablePdfCanvasColor.OfPdfCanvasColor targetColor)
                    match defaultArg overprint false with 
                    | true -> PdfCanvas.setExtGState newExtGSState
                    | false -> ()
                    match lineJoinStyle with 
                    | None -> ()
                    | Some lineJoinStyle -> PdfCanvas.setLineJoinStyle lineJoinStyle
                ]

            match info with 
            | If IIntegratedRenderInfo.isStrokeVisible ->
                match info with 
                | IIntegratedRenderInfo.Path pathInfo ->
                    let strokeWidth = pathInfo.PathRenderInfo.GetLineWidth()
                    setStroke (float strokeWidth +  width)

                | IIntegratedRenderInfo.Text textInfo ->
                    let strokeWidth = textInfo.TextRenderInfo.GetGraphicsState().GetLineWidth()
                    setStroke (float strokeWidth +  width)

                |> ModifierPdfCanvasActions.CreateActions args.Tag  

            | If IIntegratedRenderInfo.isFillVisible ->
                let addPrefix prefixActions (modifierPdfCanvasActions: ModifierPdfCanvasActions) = 
                    { modifierPdfCanvasActions with 
                        Actions = prefixActions @ modifierPdfCanvasActions.Actions
                    }

                match info with 
                | IIntegratedRenderInfo.Path pathInfo ->
                    addPrefix (setStroke (width)) (Modifier.OpenStroke() args)
                | IIntegratedRenderInfo.Text textInfo ->
                    addPrefix (setStroke (width)) (Modifier.OpenStroke() args)

            | _ -> 
                ModifierPdfCanvasActions.Keep(args.Tag)


    static member AddVarnish(varnish: FsSeparation, width) =
        Modifier.ExpandStrokeWidth(
            PdfCanvasColor.Separation varnish,
            width,
            overprint = true,
            lineJoinStyle = LineJoinStyle.ROUND
        )

    static member ReleaseCompoundPath() =
        fun (args: _SelectionModifierFixmentArguments<'userState>) ->
            match args.CurrentRenderInfo with 
            | IIntegratedRenderInfo.Text _ -> ModifierPdfCanvasActions.Keep(args.Tag)
            | IIntegratedRenderInfo.Path pathInfo ->
                
                let subPaths = pathInfo.PathRenderInfo.GetPath().GetSubpaths()
                match subPaths.Count with 
                | 0  
                | 1 -> ModifierPdfCanvasActions.Keep(args.Tag)
                | _ -> 
                    let close = pathInfo.PathRenderInfo.GetOperation()
                    let rule = pathInfo.PathRenderInfo.GetRule()
                    let suffixActions =
                        let splittedOperatorRanges = 
                            pathInfo.AccumulatedPathOperatorRanges
                            |> List.ofSeq
                            |> List.splitIfChangedWith ChangedItemIntoPosition.Next (fun _ item ->
                                match item.Operator.ToString() with 
                                | EQ m -> false
                                //| EQ l -> false
                                | _ -> true
                            )

                        splittedOperatorRanges
                        |> List.collect(fun operatorRanges ->
                            let drawActions = 
                                operatorRanges.AsList
                                |> List.map(PdfCanvas.writeOperatorRange)

                            drawActions @ [PdfCanvas.closePathByOperation close rule]
                        )
                        

                    { Actions = []
                      SuffixActions = suffixActions
                      Close = 
                        CloseOperatorUnion.CreatePath(
                            fill = CloseOperator.Close,
                            stroke = CloseOperator.Close
                        )
                    }

          

    

type SelectorAndModifiersRecordIM<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: ModifierIM<'userState> list }

type SelectorAndModifiersRecordShadableIM<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: ShadableModifierIM<'userState> list }

type SelectorAndModifiersRecord<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list }


type SelectorAndModifiersRecordEx<'userState> =
    { Name: string 
      Selector: Selector<'userState> 
      Modifiers: Modifier<'userState> list
      PageInfosValidation: PageInfosValidation 
      Parameters: list<string * string>}

type SelectorAndModifiers<'userState>(name, selector: Selector<'userState>, modifiers: Modifier<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidation.ignore
    
    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiersUnion<'userState>(name, selector: Selector<'userState>, modifiers: ModifierUnion<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidation.ignore
    
    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiersUnionIM<'userState>(name, selector: Selector<'userState>, modifiers: ModifierUnionIM<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidationIM.ignore

    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiersIM<'userState>(name, selector: Selector<'userState>, modifiers: ModifierIM<'userState> list, ?parameters: list<string * string>, ?pageInfosValidation) =
    let pageInfosValidation = defaultArg pageInfosValidation PageInfosValidationIM.ignore

    member x.Name = name

    member x.Selector = selector

    member x.Modifiers = modifiers

    member x.Parameters = parameters

    member x.PageInfosValidation = pageInfosValidation

type SelectorAndModifiers<'userState> with 
    member internal x.ToIM() =
        let modifiers =
            x.Modifiers
            |> List.map(Modifier.toIM)

        let pageInfosValidation =
            PageInfosValidationIM.ofPageInfosValidation x.PageInfosValidation

        SelectorAndModifiersIM(x.Name, x.Selector, modifiers, ?parameters = x.Parameters, pageInfosValidation = pageInfosValidation)

    member internal x.ToUnion() =
        let modifiers =
            x.Modifiers
            |> List.map(ModifierUnion<_>.Modifier)

        let pageInfosValidation = x.PageInfosValidation

        SelectorAndModifiersUnion(x.Name, x.Selector, modifiers, ?parameters = x.Parameters, pageInfosValidation = pageInfosValidation)

type SelectorAndModifiersIM<'userState> with 
    member internal x.ToUnion() =
        let modifiers =
            x.Modifiers
            |> List.map(ModifierUnionIM<_>.Modifier)

        let pageInfosValidation = x.PageInfosValidation

        SelectorAndModifiersUnionIM(x.Name, x.Selector, modifiers, ?parameters = x.Parameters, pageInfosValidation = pageInfosValidation)



type SelectorAndModifiersUnion<'userState> with 
    member internal x.ToIM() =
        let modifiers =
            x.Modifiers
            |> List.map(ModifierUnion.toIM)

        let pageInfosValidation =
            PageInfosValidationIM.ofPageInfosValidation x.PageInfosValidation

        SelectorAndModifiersUnionIM(x.Name, x.Selector, modifiers, ?parameters = x.Parameters, pageInfosValidation = pageInfosValidation)


[<AutoOpen>]
module ModifyOperators =

    [<RequireQualifiedAccess>]
    type ModifyingAsyncWorker =
        | PageNumberEveryWorker of int
        | Sync

    type PageLogger =
        { LoggerLevel: PdfLoggerLevel 
          LoggingPageCountInterval: int }
    with 
        member x.Log(text, ?alwaysPrintingConsole_If_Info) =
            fun pageNumber ->
                match x.LoggerLevel with 
                | PdfLoggerLevel.Info ->
                    let alwaysPrintingConsole_If_Info = defaultArg alwaysPrintingConsole_If_Info false
                    let logInfo text =
                        match alwaysPrintingConsole_If_Info with 
                        | true -> PdfLogger.info_alwaysPrintingInConsole (text)
                        | false -> PdfLogger.info (text)

                    match pageNumber = 1 with 
                    | true -> logInfo (text())
                    | false -> 

                        match x.LoggingPageCountInterval with 
                        | BiggerThan 1 & interval -> 
                            match pageNumber % interval with 
                            | 0 -> logInfo (text())
                            | _ -> ()
                        | _ -> ()

                | PdfLoggerLevel.Slient -> ()

    [<RequireQualifiedAccess>]
    module IntegratedDocument =
        type private Modifier = _SelectionModifierFixmentArguments -> ModifierPdfCanvasActions
        
        let private modifyCommon (config: Configuration) ops (pageSelector: PageSelector) (pageLogger: PageLogger option) (selectorModifierPageInfosValidationMappingFactory) modify (document: IntegratedDocument) =
            let selectedPageNumbers = document.Value.GetPageNumbers(pageSelector) 
            match selectedPageNumbers with 
            | [] -> ()
            | _ ->
                let totalNumberOfPages = document.Value.GetNumberOfPages()
                let ops = 
                    let ops =
                        ops
                        |> Option.orElse config.PdfModifyOptions

                    match ops with 
                    | None -> PdfModifyOptions.DefaultValue
                    | Some ops ->
                        match selectedPageNumbers.Length = totalNumberOfPages && selectedPageNumbers = [1..totalNumberOfPages] with 
                        | true -> ops
                        | false ->
                            { ops with XObjectReference = XObjectReference.ByCopied }
                        
                let refText = 
                    match ops.XObjectReference with 
                    | XObjectReference.ByCopied -> ""
                    | XObjectReference.ByRef -> "[ref]"

                for pageNumber = 1 to totalNumberOfPages do
                    if List.contains pageNumber selectedPageNumbers then
                        let page = document.Value.GetPage(pageNumber)
                        let selectorModifierMapping: Map<SelectorModiferToken, _> = selectorModifierPageInfosValidationMappingFactory (PageNumber pageNumber, page)
                        for pair in selectorModifierMapping do
                            match pageLogger with 
                            | None -> 
                                let renderInfoSelector, modifier, pageInfosValidation = pair.Value
                                pageInfosValidation (PageNumber pageNumber) (modify ops (Map.ofList[pair.Key, (renderInfoSelector, modifier)]) page)
                            | Some pageLogger -> 
                                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                                let renderInfoSelector, modifier, pageInfosValidation = pair.Value
                                let r = pageInfosValidation (PageNumber pageNumber) (modify ops (Map.ofList[pair.Key, (renderInfoSelector, modifier)]) page)
                                stopWatch.Stop()
                                let names =
                                    selectorModifierMapping
                                    |> Map.toList
                                    |> List.map(fun (m, _) -> m.Name)
                                    |> String.concat "; "

                                pageLogger.Log(
                                    (fun () -> sprintf "%srun %s %d in %O" refText names pageNumber stopWatch.Elapsed),
                                    alwaysPrintingConsole_If_Info = true
                                ) pageNumber

                                r

                (document.Value :> IFsPdfDocumentEditor).Resources.DeleteRemovableXObject(ops)

        let modify config pdfModifyOptions (pageSelector: PageSelector) pageLogger (selectorModifierPageInfosValidationMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * ModifierUnion * (PageNumber -> IIntegratedRenderInfo seq -> unit)>) (document: IntegratedDocument) =
            modifyCommon   
                config
                pdfModifyOptions
                pageSelector 
                pageLogger 
                selectorModifierPageInfosValidationMappingFactory 
                (PdfPage.modify)
                document

        let modifyIM config ops (pageSelector: PageSelector) pageLogger (selectorModifierPageInfosValidationMappingFactory: (PageNumber * PdfPage) -> Map<SelectorModiferToken, RenderInfoSelector * ModifierUnion * (PageNumber -> IIntegratedRenderInfoIM seq -> unit)>) (document: IntegratedDocument) =
            modifyCommon
                config
                ops
                pageSelector
                pageLogger
                selectorModifierPageInfosValidationMappingFactory
                (PdfPage.modifyIM)
                document
            


    [<RequireQualifiedAccess>]
    module private Manipulate =
        let runInAsync (modifyingAsyncWorker: ModifyingAsyncWorker) f  =
            fun (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                let totalNumberOfPages = document.Value.GetNumberOfPages()
                match modifyingAsyncWorker, totalNumberOfPages with 
                | ModifyingAsyncWorker.Sync, _ 
                | _ , 1 -> f totalNumberOfPages id flowModel document
                | ModifyingAsyncWorker.PageNumberEveryWorker i, _ when i < 1 -> failwith "Async worker number should bigger than 1"
                | ModifyingAsyncWorker.PageNumberEveryWorker i, j when i > 0 && j > 1 ->
                    failwithf "Async worker is disabled now"
                    let splitedFlowModels = 
                        runWithFlowModel flowModel (Flow.FileOperation (FileOperations.splitDocumentToMany (fun args -> { args with Override = true; ChunkSize = i})))


                    let flowModels = 
                        splitedFlowModels
                        |> List.chunkBySize i
                        |> List.mapi (fun groupIndex flowModels -> 
                            async {
                                return
                                    flowModels
                                    |> List.mapi (fun memberIndex flowModel ->
                                        let pageNum = groupIndex * i + (memberIndex + 1)
                                        let manipuate = (Manipulate (f totalNumberOfPages (fun _ -> PageNumber pageNum)))
                                        runWithFlowModel flowModel (Flow.Manipulate manipuate)
                                    )
                                    |> List.concat
                            }
                        )
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> List.concat

                    let mergeFlow = 
                        Flow.FileOperation 
                            (FileOperations.mergeDocumentsInternal flowModel.Configuration flowModel.File (document.Value))


                    runManyWithFlowModels flowModel.Configuration flowModels mergeFlow
                    |> ignore

                    for flowModel in flowModels do
                        File.Delete(flowModel.File)

                    flowModels.[0].UserState


                | _ -> failwith "Invalid token"
            |> Manipulate



    /// async may doesn't make any sense for cpu bound computation?
    let private modifyIM (modifyingAsyncWorker, pageSelector, loggingPageCountInterval, (selectorAndModifiersList: list<SelectorAndModifiersUnionIM<'userState>>), ops) =
        let names = 
            selectorAndModifiersList
            |> List.map (fun selectorAndModifier -> selectorAndModifier.Name)

        if names.Length <> (List.distinct names).Length then failwithf "Duplicated keys in SelectorAndModifiers %A" selectorAndModifiersList

        let asyncManiputation (selectorAndModifiersList: SelectorAndModifiersUnionIM<_> list) = 
            fun (totalNumberOfPages) (transformPageNum: PageNumber -> PageNumber) (flowModel: FlowModel<_>) (document: IntegratedDocument) ->
                IntegratedDocument.modifyIM
                    flowModel.Configuration
                    (ops)
                    pageSelector 
                    (loggingPageCountInterval |> Option.map(fun loggingPageCountInterval ->
                        { LoggingPageCountInterval = loggingPageCountInterval 
                          LoggerLevel = flowModel.Configuration.LoggerLevel }
                    ))
                    (
                        fun (pageNum, pdfPage) ->
                            selectorAndModifiersList 
                            |> List.mapi (fun i (selectorAndModifiers) ->
                                let pageModifingArguments =
                                    { PageNum = (transformPageNum pageNum).Value
                                      UserState = flowModel.UserState
                                      Page = pdfPage
                                      TotalNumberOfPages = totalNumberOfPages }
                                ( { Name = selectorAndModifiers.Name }, 
                                    ( Selector.toRenderInfoSelector pageModifingArguments selectorAndModifiers.Selector,
                                      Modifiers.toSelectionModifierUnionIM pageModifingArguments selectorAndModifiers.Modifiers,
                                      selectorAndModifiers.PageInfosValidation.Value
                                    )
                                )
                            )
                            |> Map.ofList
                    ) document

        selectorAndModifiersList
        |> List.map (fun modifier ->
            let parameters =
                match modifier.Parameters with 
                | None ->
                    [
                        "pageSelector" => pageSelector.ToString()
                        "selector" => modifier.Selector.ToString()
                    ]

                | Some parameters ->
                    (
                        [
                            "pageSelector" => pageSelector.ToString()
                            "selector" => modifier.Selector.ToString()
                        ]
                        @ parameters
                    )
                    |> List.distinctBy(fun (key, _) -> key.Trim().ToLower())


            Manipulate.runInAsync modifyingAsyncWorker (asyncManiputation [modifier])
            |> Manipulate.rename 
                modifier.Name
                parameters
        )
        |> Manipulate.Batch()
        ||>> ignore
        
    /// async may doesn't make any sense for cpu bound computation?
    let private modify (modifyingAsyncWorker, pageSelector, loggingPageCountInterval, (selectorAndModifiersList: list<SelectorAndModifiersUnion<'userState>>), ops) =
        modifyIM (modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList |> List.map (fun m -> m.ToIM()), ops)


    type Modify =
        static member Create(pageSelector, selectorAndModifiersList: SelectorAndModifiers<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList = 
                selectorAndModifiersList
                |> List.map(fun m -> m.ToUnion())

            modify(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)

        static member CreateIM(pageSelector, selectorAndModifiersList: SelectorAndModifiersIM<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList = 
                selectorAndModifiersList
                |> List.map(fun m -> m.ToUnion())

            modifyIM(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)


        static member Create (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecord<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers).ToUnion()
                )
            modify(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)

        static member Create_Record (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecord<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            Modify.Create(pageSelector, selectorAndModifiersList, ?modifyingAsyncWorker = modifyingAsyncWorker, ?loggingPageCountInterval = loggingPageCountInterval, ?ops = ops)

        static member Create_RecordIM (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordIM<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiersIM(m.Name, m.Selector, m.Modifiers).ToUnion()
                )
            modifyIM(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)

        static member Create_Record_Shadarable (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordShadableIM<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiersUnionIM(m.Name, m.Selector, List.map ModifierUnionIM.ShadingModifier m.Modifiers)
                )
            modifyIM(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)


        static member Create_RecordEx (pageSelector, selectorAndModifiersList: SelectorAndModifiersRecordEx<'userState> list, ?modifyingAsyncWorker, ?loggingPageCountInterval, ?ops) =
            let modifyingAsyncWorker = defaultArg modifyingAsyncWorker ModifyingAsyncWorker.Sync
            let selectorAndModifiersList =
                selectorAndModifiersList
                |> List.map (fun m ->
                    SelectorAndModifiers(m.Name, m.Selector, m.Modifiers, m.Parameters, m.PageInfosValidation).ToUnion()
                )
            modify(modifyingAsyncWorker, pageSelector, loggingPageCountInterval, selectorAndModifiersList, ops)

type FontAndSizeQuery [<JsonConstructor>] (?fontNames, ?fontSize, ?fillColor, ?info_BoundIs_Args, ?textPattern, ?wordSep) =
    inherit POCOBaseEquatable<option<string list> * float option * FsColor option * Info_BoundIs_Args option * TextSelectorOrTransformExpr option * string option>(fontNames, fontSize, fillColor, info_BoundIs_Args, textPattern, wordSep)
    let fontNamesQuery =
        match fontNames with 
        | None -> None
        | Some fontNames -> AtLeastOneList.TryCreate fontNames


    [<JsonProperty>]
    member x.FontNames = fontNames

    [<JsonProperty>]
    member x.FontSize = fontSize

    [<JsonProperty>]
    member x.FillColor = fillColor

    [<JsonProperty>]
    member x.Info_BoundIs_Args = info_BoundIs_Args

    [<JsonProperty>]
    member x.TextPattern = textPattern

    static member MethodConversion(?fontNames, ?fontSize: float, ?fillColor: AlternativeFsColor, ?leftBottomBasedInboxArea: MMRectangle, ?textPattern: string): MethodLiteralConversion<_> =
        let name = nameof(FontAndSizeQuery)
        let fontNames =
            match fontNames with 
            | None -> None
            | Some fontNames ->
                fontNames
                |> String.concat "☆"
                |> Some

        let alternativeFsColorText = 
            match fillColor with 
            | Some (color) -> color.LoggingText_Raw
            | None -> ""

        let inboxAreaText =
            match leftBottomBasedInboxArea with 
            | None -> ""
            | Some inboxArea -> inboxArea.LoggingText


        {
            MethodLiteral = 
                { Name = name
                  Parameters = 
                    [
                        nameof(fontNames)    ==> defaultArg fontNames ""
                        nameof(fontSize)    ==> defaultArg fontSize 0.
                        nameof(fillColor)   ==> alternativeFsColorText
                        nameof(leftBottomBasedInboxArea)   ==> inboxAreaText
                        nameof(textPattern) ==> defaultArg textPattern ""

                    ]
                    |> Observations.Create
                  }

            OfMethodLiteral = (fun (method: MethodLiteral) ->
                match method.Name with 
                | EqualTo name ->
                    let fontNames = 
                        method.Parameters.[nameof(fontNames)].Value.Text
                        |> String.asOption
                        |> Option.map(fun m -> m.SplitAsListAndTrim("☆"))

                    let fontSize= 
                        method.Parameters.[nameof(fontSize)].Value.Text
                        |> System.Double.Parse
                        |> function 
                            | 0. -> None
                            | fontSize -> Some fontSize

                    let fillColor = 
                        method.Parameters.[nameof(fillColor)].Value.Text
                        |> String.asOption
                        |> Option.map(AlternativeFsColor.OfLoggingText_Raw >> FsColor.OfAlternativeFsColor)

                    let inboxArea = 
                        method.Parameters.[nameof(leftBottomBasedInboxArea)].Value.Text
                        |> String.asOption
                        |> Option.map(MMRectangle.Parse)
                        |> Option.map(fun rect ->
                            Info_BoundIs_Args(RelativePosition.Inbox, AreaGettingOptions.FsSpecfic rect.AsFsRectangle)
                        )

                    let textPattern =
                        match method.Parameters.[nameof(textPattern)].Value.Text.Trim() with 
                        | "" -> None
                        | text ->
                            TextSelectorOrTransformExpr.Parse text
                            |> Some

                    FontAndSizeQuery(
                        ?fontNames = fontNames,
                        ?fontSize = fontSize,
                        ?fillColor = fillColor,
                        ?info_BoundIs_Args = inboxArea,
                        ?textPattern = textPattern
                    )
                    |> Some
                    
                | _ -> None
            )
        }

    member v.MethodLiteralText =
        FontAndSizeQuery.MethodConversion(
            ?fontNames = v.FontNames,
            ?fontSize = v.FontSize, 
            ?fillColor = (v.FillColor |> Option.bind (fun m -> m.AsAlternativeFsColor)),
            ?textPattern = (v.TextPattern |> Option.map(fun m -> m.MethodLiteralText)),
            ?leftBottomBasedInboxArea = (
                match v.Info_BoundIs_Args with 
                | None -> None
                | Some v ->
                    match v.AreaGettingOptions with 
                    | AreaGettingOptions.FsSpecfic v -> Some (v.MMValue)
                    | _ -> None
            )
        ).MethodLiteralText

    static member MethodConversion_Parse(text) =
        MethodLiteral.TryParse text
        |> Result.bind(fun r ->
            match FontAndSizeQuery.MethodConversion().OfMethodLiteral(r) with 
            | None -> Result.Error (sprintf "Cannot parse MethodLiteral %s to FontAndSizeQuery" r.Text)
            | Some r -> Result.Ok r
        )

    member x.LoggingText =
        let fontSize = 
            match fontSize with 
            | Some fontSize -> Some (fontSize.ToString())
            | None -> None

        let fillColor =
            match fillColor with 
            | Some fillColor -> Some fillColor.LoggingText
            | None -> None

        let textPattern = textPattern |> Option.map (fun m -> m.ToString())
        let info_BoundIs_Args = info_BoundIs_Args |> Option.map(fun m -> m.ToString())

        let fontNames =
            match fontNames with 
            | None -> None
            | Some fontNames ->
                fontNames
                |> String.concat "☆"
                |> Some

        [ fontNames 
          fontSize 
          fillColor
          textPattern
          info_BoundIs_Args ]
        |> List.choose id
        |> List.filter(fun m -> m <> "")
        |> String.concat " "

    member x.PredicateByInBoxTextInfo(text2, fontName2: DocumentFontName, fontSize2: float, fillColor2: FsColor, leftBottomBasedBound: Rectangle) =
        match textPattern with 
        | Some textPattern -> textPattern.Predicate text2
        | None -> true
        &&
        match fontNamesQuery, fontSize with 
        | Some fontNames, Some fontSize -> 
            fontNames.AsList
            |> List.exists(fun fontName ->
                fontName2.SameFontNameTo(fontName)
            )
            && fontSize2 @= fontSize

        | Some fontNames, None -> 
            fontNames.AsList
            |> List.exists(fun fontName ->
                fontName2.SameFontNameTo(fontName)
            )

        | None, Some fontSize -> fontSize2 @= fontSize
        | None, None -> true 
        && ( match fillColor with 
              | Some fillColor -> 
                 FsColor.equal (fillColor) fillColor2
              | None -> true
           )
        &&
        match info_BoundIs_Args with 
        | Some args2 ->
            match args2.AreaGettingOptions with 
            | AreaGettingOptions.FsSpecfic leftBottomBasedRect -> leftBottomBasedBound.IsInsideOf leftBottomBasedRect.AsRectangle
            | _ -> true
        | None -> true


    member x.AsSelector() = 
        Selector.Text(fun args textInfo ->
            match textPattern with 
            | Some textPattern -> 
                let text = ITextRenderInfo.getConcatedText wordSep textInfo
                textPattern.Predicate text
            | None -> true
            &&
            match fontNamesQuery, fontSize with 
            | Some fontNames, Some fontSize -> 
                fontNames.AsList
                |> List.exists(fun fontName ->
                    TextInfo.FontNameAndSizeIs(fontName, fontSize) args textInfo
                )
            | Some fontNames, None -> 
                fontNames.AsList
                |> List.exists(fun fontName ->
                    TextInfo.FontNameIs(fontName) args textInfo
                )

            | None, Some fontSize -> TextInfo.FontSizeIs(fontSize) args textInfo
            | None, None -> true 
            && ( match fillColor with 
                  | Some fillColor -> 
                     let fillColor' = textInfo.TextRenderInfo.GetFillColor() |> FsColor.OfItextColor
                     FsColor.equal (fillColor) fillColor'
                  | None -> true
               )
            &&
            match info_BoundIs_Args with 
            | Some args2 ->
                let args2 = args2
                Info.DenseBoundIs(
                    args2.RelativePosition,
                    args2.AreaGettingOptions,
                    args2.BoundGettingStrokeOptions
                ) args textInfo

            | None -> true

        )

[<RequireQualifiedAccess>]
type FontAndSizeQueryUnion =
    | Value of FontAndSizeQuery
    | AND of FontAndSizeQueryUnion list
    | OR of FontAndSizeQueryUnion list
with 
    member x.AsSelector() =
        match x with 
        | FontAndSizeQueryUnion.Value v -> v.AsSelector()
        | FontAndSizeQueryUnion.AND vs ->
            vs
            |> List.map(fun v -> v.AsSelector())
            |> Selector.AND
        | FontAndSizeQueryUnion.OR vs ->
            vs
            |> List.map(fun v -> v.AsSelector())
            |> Selector.OR

    static member Arial =
        let fontNames = 
            FontNames.Query.Arial_Regular_Names
            |> List.map(fun m -> m.Text())
        FontAndSizeQuery(fontNames)

type FontMapping =
    { FontAndSizeQuery: FontAndSizeQueryUnion
      NewFontAndSize: NewFontAndSize }
with 
    static member Arial_Regular_To_Bold =
        let query = 
            let fontNames = 
                FontNames.Query.Arial_Regular_Names
                |> List.map(fun m -> m.Text())

            FontAndSizeQuery(fontNames)
            |> FontAndSizeQueryUnion.Value

        let newFontAndSize = 
            let newFont = FsPdfFontFactory.Registerable(Arial.arial (Arial.FontWeight.Bold))
            NewFontAndSize(newFont)

        { FontAndSizeQuery = query 
          NewFontAndSize   = newFontAndSize }

    static member Arial_Bold_To_Regular =
        let query = 
            let fontNames = 
                FontNames.Query.Arial_Bold_Names
                |> List.map(fun m -> m.Text())

            FontAndSizeQuery(fontNames)
            |> FontAndSizeQueryUnion.Value

        let newFontAndSize = 
            let newFont = FsPdfFontFactory.Registerable(Arial.arial (Arial.FontWeight.Regular))
            NewFontAndSize(newFont)

        { FontAndSizeQuery = query 
          NewFontAndSize   = newFontAndSize }

type FontMappings = FontMappings of FontMapping al1List
with 
    member x.AsAl1List =
        let (FontMappings v) = x
        v

    member x.AsList = x.AsAl1List.AsList

type NewFontAndSize with 
    member x.AsModifier(): Modifier<_> =
        Modifier.NewFontAndSize(x)

    
type TextStyle [<JsonConstructor>] private (v) =
    inherit POCOBaseV<VectorStyle option * NewFontAndSize option * string option>(v)

    member x.VectorStyle = x.VV.Item3_1()

    member x.NewFontAndSize = x.VV.Item3_2()

    member x.NewText = x.VV.Item3_3()

    member x.AsModifier() =
        fun args ->
            let r = 
                [
                    match x.VectorStyle with 
                    | Some style -> (style.AsModifier args)
                    | None ->       ()

                    match x.NewFontAndSize with 
                    | Some newFontAndSize -> (newFontAndSize.AsModifier() args)
                    | None -> ()
                ]
                |> ModifierPdfCanvasActions.ConcatOrKeep args.Tag 

            match x.NewText with 
            | None -> r
            | Some newText ->
                let newText =
                    { PdfConcatedWord.HeadWord = newText 
                      FollowedWords = [] }

                { r with
                    Close = CloseOperatorUnion.CreateText(text = newText)
                }



    new (?vectorStyle, ?newFontAndSize, ?newText) =
        DefaultArgs.CheckAllInputsAreNotEmpty(vectorStyle, newFontAndSize, newText)
        new TextStyle((vectorStyle, newFontAndSize, newText))

type Modifier with 
    static member ChangeTextStyle(textStyle: TextStyle) =
        textStyle.AsModifier()

type Modify_ReplaceColors_Options = 
    { FillOrStrokeOptions: FillOrStrokeOptions 
      PageInfosValidation: PageInfosValidation 
      SelectorTag: SelectorTag
      Info_BoundIs_Args: Info_BoundIs_Args option
      PageSelector: PageSelector }
with 
    static member DefaultValue =
        { FillOrStrokeOptions = FillOrStrokeOptions.FillOrStroke 
          PageInfosValidation = PageInfosValidation.ignore
          SelectorTag = SelectorTag.PathOrText 
          Info_BoundIs_Args = None
          PageSelector = PageSelector.All }

type Modify =
    static member ExpandStrokeWidth(colors, width: ``ufloat>0``, targetColor, ?lineJoinStyle, ?overprint) =
        Modify.Create(
            PageSelector.All,
            [
                SelectorAndModifiers(
                    name = sprintf "Expand stroke width %g" width.Value,
                    selector = (
                        Selector.PathOrText(
                            (Info.IsStrokeVisible() <&&> Info.StrokeColorIsOneOf colors)
                            <||>
                            (Info.IsFillVisible() <&&> Info.FillColorIsOneOf colors)
                        )
                    ),
                    modifiers = [
                        Modifier.ExpandStrokeWidth(targetColor, width, ?lineJoinStyle =lineJoinStyle, ?overprint = overprint)
                    ]
                )
            ]
        )

    static member AddVarnish(selector, varnish: FsSeparation, width: ``ufloat>0``) =
        Modify.Create(
            PageSelector.All,
            [
                SelectorAndModifiers(
                    name = sprintf "add varnish '%s' width %g" varnish.LoggingText width.Value,
                    selector = selector,
                    modifiers = [
                        Modifier.AddVarnish(varnish, width)
                    ]
                )
            ]
        )

    static member AddBoundToTexts(?selector, ?canvasAddRectangleArgs) =
        let canvasAddRectangleArgs =
            match canvasAddRectangleArgs with 
            | None -> fun (args: PdfCanvasAddRectangleArguments) -> 
                { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
            | Some args -> args
                
        Modify.Create(
            PageSelector.All,
            [
                { Name = "add bound to texts"
                  Selector = Text(defaultArg selector (fun _ _ -> true)) 
                  Modifiers = [
                    Modifier.AddRectangleToBound(canvasAddRectangleArgs)
                  ]
                }
            ]
        )

    static member private ReplaceColorsCommon (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
        let fillOrStrokeOptions = options.FillOrStrokeOptions

        let selectorTag = options.SelectorTag

        let pageInfosValidation = options.PageInfosValidation

        let fillOrStrokeModifyingOptions =
            match fillOrStrokeOptions with 
            | FillOrStrokeOptions.FillAndStroke 
            | FillOrStrokeOptions.FillOrStroke -> FillOrStrokeModifingOptions.FillAndStroke
            | FillOrStrokeOptions.Fill -> FillOrStrokeModifingOptions.Fill
            | FillOrStrokeOptions.Stroke -> FillOrStrokeModifingOptions.Stroke
        
        let nameAndParameters =
            defaultArg 
                nameAndParameters 
                { Name = "ReplaceColors"
                  Parameters = [ "Modify_ReplaceColors_Options" => options.ToString()] }

        Modify.Create(
            options.PageSelector,
            [
                SelectorAndModifiers(
                    name = nameAndParameters.Name,
                    selector =
                        (
                            let colorInfo = 
                                fun args info ->
                                    Info.ColorIs(fillOrStrokeOptions, fun color -> 
                                        match picker (args, info) color with 
                                        | Some _ -> true
                                        | None -> false
                                    ) args info

                            let info =
                                match options.Info_BoundIs_Args with 
                                | None -> colorInfo
                                | Some info_boundIs_args -> Info.BoundIs(info_boundIs_args) <&&> colorInfo

                            match selectorTag with 
                            | SelectorTag.PathOrText -> PathOrText info
                            | SelectorTag.Path -> Selector.Path info
                            | SelectorTag.Text -> Text info
                        ),
                    modifiers = [
                        Modifier.ReplaceColorCommon(
                            fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                            picker = (fun args color ->
                                picker (args.PageModifingArguments, args.CurrentRenderInfo) color
                            )
                        )
                    ],
                    pageInfosValidation = pageInfosValidation,
                    parameters = nameAndParameters.Parameters
                )
            ]
        )
    static member ReplaceColorsEx (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        Modify.ReplaceColorsCommon(picker = (fun info color -> picker info color |> Option.map Choice1Of2), ?options = options, ?nameAndParameters = nameAndParameters)

    static member ReplaceColors (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        Modify.ReplaceColorsEx(picker = (fun info color -> picker color), ?options = options, ?nameAndParameters = nameAndParameters)
    
    static member ReplaceAlternativeColors (picker, ?options: Modify_ReplaceColors_Options, ?nameAndParameters: NameAndParameters) =
        let picker (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some color -> picker color
            | None -> None

        Modify.ReplaceColors(picker, ?options = options, ?nameAndParameters = nameAndParameters)

    static member ReplaceColors (originColors: Color list, targetColor: Color, ?options: Modify_ReplaceColors_Options) =
        let originColors = 
            originColors
            |> List.map FsColor.OfItextColor
            
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
        let nameAndParameters =
            { NameAndParameters.Name = "ReplaceColors"
              Parameters = 
                ["originColors" => originColors.ToString() 
                 "targetColor" => targetColor.ToString() 
                 "options" => options.ToString() ]
            }
        

        Modify.ReplaceColors(
            options = options,
            nameAndParameters = nameAndParameters,
            picker = fun color ->
                if FsColors.contains color originColors
                then Some targetColor
                else None
        ) :> Manipulate<_, _>


    static member ReplaceColors (originColors: Color list, targetColor: FsSeparation, ?options: Modify_ReplaceColors_Options) =
        Manipulate.Factory(fun flowModel splitDocument ->
            let seperationColor = splitDocument.Value.GetOrCreateColor(ResourceColor.CustomSeparation targetColor)
            Modify.ReplaceColors(originColors, seperationColor, ?options = options)
        )

    static member ReplaceColors (colorMapping: ColorMappings, ?options: Modify_ReplaceColors_Options) =
        Manipulate.Factory(fun flowModel splitDocument ->
            let picker = 
                colorMapping.AsPicker() 
                
            let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue
            let nameAndParameters =
                { Name = "ReplaceColors"
                  Parameters = 
                    ["colorMapping" => colorMapping.ToString()
                     "options" => options.ToString() ]
                }


            Modify.ReplaceColorsCommon((fun args color -> picker color |> Option.map Choice2Of2), options = options, nameAndParameters = nameAndParameters)
        )

    static member ReplaceColors1 (colorMapping: ColorMappings, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(colorMapping, ?options = options)

    static member ReplaceColors1 (originColors: Color list, targetColor: Color, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(originColors, targetColor, ?options = options)

    static member ReplaceColors1 (originColors: Color list, targetColor: FsSeparation, ?options: Modify_ReplaceColors_Options) =
        let options =
            options 
            |> Option.map (fun options ->
                { options with  
                    PageInfosValidation = PageInfosValidation.atLeastOne }
            )

        Modify.ReplaceColors(originColors, targetColor, ?options = options)


    static member InvertColors(?predicate: FsColor -> bool, ?options: Modify_ReplaceColors_Options) =
        let options = defaultArg options Modify_ReplaceColors_Options.DefaultValue

        let nameAndParameters =
            { Name = "InvertColors" 
              Parameters = 
                ["predicate" => predicate.ToString()
                 "options" => options.ToString() ]
            }

        let predicate = defaultArg predicate (fun _ -> true)

        Modify.ReplaceColors(
            options = options,
            picker = (
                fun color ->
                    if predicate color 
                    then
                        (color.AlterColor)
                        |> Option.map(
                            FsValueColor.Invert
                            >> FsValueColor.ToItextColor
                        )

                    else None
            ),
            nameAndParameters = nameAndParameters
        )

    static member DecodeText() =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { Name = "DecodeTexts"
                  Selector = Selector.Text(fun _ _ -> true)
                  Modifiers = 
                    [
                        Modifier.DecodeText()
                    ]
                  PageInfosValidation = PageInfosValidation.ignore
                  Parameters = []
                }
            ]
        )

    static member MapFontAndSizeF(fOldFontAndSize: PageModifingArguments<_> -> FontAndSizeQueryUnion, newFontAndSize: NewFontAndSize) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { Name = "MapFonts"
                  Selector = Selector.Factory(fun args -> (fOldFontAndSize args).AsSelector())
                  Modifiers = 
                    [
                        newFontAndSize.AsModifier()
                    ]
                  PageInfosValidation = PageInfosValidation.ignore
                  Parameters = 
                    ["oldFontAndSize" => fOldFontAndSize.ToString()
                     "newFontAndSize" => newFontAndSize.ToString() ]
                }
            ]
        )

    static member MapFontAndSizeF(fOldFontAndSize: PageModifingArguments<_> -> FontAndSizeQuery, newFontAndSize: NewFontAndSize) =
        Modify.MapFontAndSizeF(
            fOldFontAndSize = (fun args ->fOldFontAndSize args |> FontAndSizeQueryUnion.Value),
            newFontAndSize = newFontAndSize
        )

    static member MapFontAndSize(oldFontAndSize: FontAndSizeQuery, newFontAndSize: NewFontAndSize) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { Name = "MapFonts"
                  Selector = Selector.Factory(fun args -> oldFontAndSize.AsSelector())
                  Modifiers = 
                    [
                        newFontAndSize.AsModifier()
                    ]
                  PageInfosValidation = PageInfosValidation.ignore
                  Parameters = 
                    ["oldFontAndSize" => oldFontAndSize.LoggingText
                     "newFontAndSize" => newFontAndSize.ToString() ]
                }
            ]
        )



    static member SplitTextLineToWords() =
        Modify.Create_Record(
            PageSelector.All,
            selectorAndModifiersList = [
                { SelectorAndModifiersRecord.Name = "split text line to words"
                  Selector = Text(fun _ _ -> true)
                  Modifiers = [
                    (fun args ->
                        let currentInfo = args.CurrentRenderInfo :?> IntegratedTextRenderInfo

                        let font = ITextRenderInfo.getFontName currentInfo

                        match font with 
                        | DocumentFontName.Invalid -> 
                            ModifierPdfCanvasActions.Keep(args.CurrentRenderInfo.Tag)

                        | DocumentFontName.Valid _ ->
                            let textInfos = 
                                currentInfo.SplitToWords()

                            let lastTextInfo =
                                match textInfos with
                                | []
                                | [_] -> (args.CurrentRenderInfo :?> IntegratedTextRenderInfo).TextRenderInfo
                                | _ -> textInfos |> List.last |> fun m -> m.TextRenderInfo

                            let actions =
                                match textInfos with
                                | []
                                | [_] ->
                                    let matrix = lastTextInfo.GetTextMatrix()
                                    [PdfCanvas.setTextMatrix matrix]   

                                | _ ->
                                    textInfos
                                    |> List.mapi (fun i textInfo ->
                                        let matrix = textInfo.TextRenderInfo.GetTextMatrix()
                                        let lastIndex = (textInfos.Length - 1)
                                        match i with 
                                        | 0 -> 
                                            [
                                                PdfCanvas.setTextMatrix matrix
                                                PdfCanvas.showText(ITextRenderInfo.getConcatedText None textInfo)
                                            ]
                                        | EqualTo lastIndex -> 
                                            [
                                                PdfCanvas.setTextMatrix(matrix)
                                            ]
                                        | _ ->
                                            [
                                                PdfCanvas.setTextMatrix matrix
                                                PdfCanvas.showText(ITextRenderInfo.getConcatedText None textInfo)
                                            ]
                                    )
                                    |> List.concat

                            { ModifierPdfCanvasActions.Actions = actions 
                              
                              SuffixActions = []
                              Close = 
                                match textInfos with 
                                | []
                                | [ _ ] -> CloseOperatorUnion.Keep(args.CurrentRenderInfo.Tag)
                                | _ -> CloseOperatorUnion.CreateText(text = (lastTextInfo.GetText() |> PdfConcatedWord.Create) )
                            }
                        )
                  ]
                  }
            ]
        )


    static member ChangeStrokeStyle(selector, targetStyle) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { SelectorAndModifiersRecordEx.Name = "change stroke style"
                  Selector = selector
                  Modifiers = [
                    Modifier.ChangeStrokeStyle(targetStyle)
                  ]

                  Parameters = [
                    "selector" =>    selector.ToString()
                    "targetStyle" => targetStyle.ToString()
                  ]
                  PageInfosValidation = PageInfosValidation.ignore
                  }
            ]
        )
        
    static member ChangeStyleF(selector, fTargetStyle) =
        Modify.Create_RecordEx(
            PageSelector.All,
            selectorAndModifiersList = [
                { SelectorAndModifiersRecordEx.Name = "changeStyleF"
                  Selector = selector
                  PageInfosValidation = PageInfosValidation.ignore
                  Modifiers = [
                    Modifier.ChangeStyle(fTargetStyle)
                  ]
                  Parameters = [
                    "selector" =>    selector.ToString()
                    "targetStyle" => fTargetStyle.ToString()
                  ]
                  }
            ]
        )



    static member ChangeStyle(selector, targetStyle) =
        Modify.ChangeStyleF(selector, fun _ -> targetStyle)
        |> Manipulate.rename
            "ChangeStyle"
            [
                "selector" =>    selector.ToString()
                "targetStyle" => targetStyle.ToString()
              ]





    static member OpenFill(selector, ?fillColor) =
        Modify.ChangeStyle(
            selector,
            VectorStyle(
                fill = 
                    FillStyle(
                        ColorStyle(
                            color = NullablePdfCanvasColor.OfPdfCanvasColor(defaultArg fillColor PdfCanvasColor.BLACK),
                            closeOperator = CloseOperator.Open
                )
            ))
        )

    static member CancelFillAndStroke(selector, ?pageSelector) =
        Modify.Create_Record(
            defaultArg pageSelector PageSelector.All,
            [
                { SelectorAndModifiersRecord.Name = "CancelFillAndStroke"
                  Selector = selector
                  Modifiers = 
                    [
                        Modifier.CancelFillAndStroke()
                    ]
                  }
            ]
        )

    static member CancelStroke(selector, ?pageSelector) =
        Modify.Create_Record(
            defaultArg pageSelector PageSelector.All,
            [
                { SelectorAndModifiersRecord.Name = "CancelStroke"
                  Selector = selector
                  Modifiers = 
                    [
                        Modifier.CancelStroke()
                    ]
                  }
            ]
        )

    static member CancelFill(selector, ?pageSelector) =
        Modify.Create_Record(
            defaultArg pageSelector PageSelector.All,
            [
                { SelectorAndModifiersRecord.Name = "CancelFill"
                  Selector = selector
                  Modifiers = 
                    [
                        Modifier.CancelFill()
                    ]
                  }
            ]
        )


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
                                
                                    let pdfCanvas = new PdfCanvas(args.Page)

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

                                        let pdfCanvas = new PdfCanvas(args.Page.NewContentStreamBefore(), args.Page.GetResources(), args.Page.GetDocument())

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



