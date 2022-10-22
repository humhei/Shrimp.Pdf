namespace Shrimp.Pdf

open Newtonsoft.Json
open iText.IO.Font
open System.Collections.Generic
open System
open System.Text
#nowarn "0104"
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Math



[<AutoOpen>]
module ExtensionTypes =
    [<AutoOpen>]
    module _Colors =
        open iText.Kernel.Colors
        open iText.Kernel.Pdf.Colorspace
        

        type PdfShadingColor(shading: PdfShading, colorSpace: PdfColorSpace, ctm: Matrix) = 
            inherit Color(colorSpace, [||])

            //member x.ColorSpace = colorSpace

            member x.Ctm = ctm

            member x.Shading = shading

            new (shading: PdfShading, ctm) =
                let colorSpace = 
                    match shading.GetColorSpace() with 
                    | :? PdfName as pdfName ->
                        match pdfName with 
                        | EqualTo PdfName.DeviceCMYK -> PdfDeviceCs.Cmyk() :> PdfColorSpace
                        | EqualTo PdfName.DeviceGray -> PdfDeviceCs.Gray() :> PdfColorSpace
                        | EqualTo PdfName.DeviceRGB  -> PdfDeviceCs.Rgb() :> PdfColorSpace

                    | :? PdfArray as array ->
                        PdfSpecialCs.NChannel(array) :> PdfColorSpace

                new PdfShadingColor(shading, colorSpace, ctm)

        type PdfShadingPathRenderInfo(color: PdfShadingColor, canvasTagHierarchy, gs, path) =
            inherit PathRenderInfo(canvasTagHierarchy, gs, path, PathRenderInfo.FILL)


            member x.ShadingColor = color

    [<RequireQualifiedAccess>]
    type DecodedPdfNamePart =
        | Literal of char
        | Hex of char * char
    with 
        member x.ToByte() =
            match x with 
            | Literal char ->
                System.Convert.ToByte(char)

            | Hex (hex1, hex2) ->
                let string = System.String [|hex1; hex2|]
                System.Convert.ToByte(string, 16)

    /// decoding
    type DecodedPdfName [<JsonConstructor>] private (v) =
        inherit POCOBaseV<string>(v)
        let v2 =
            match v.TryReplaceStarting("/", "") with 
            | Some v -> v
            | None -> failwithf "An encoded PdfName %s should starts with \/" v


        let parts =
            let rec loop accum (chars: char list) =
                match chars with 
                | char :: t ->
                    match char with 
                    | '#' ->
                        let token = 
                            DecodedPdfNamePart.Hex(chars.[1], chars.[2])

                        loop (token :: accum) chars.[3..]
                    | _ -> 
                        loop (DecodedPdfNamePart.Literal char :: accum) t

                | [] -> List.rev accum
                    
            loop [] (v2.ToCharArray() |> List.ofArray)

        let bytes = 
            parts
            |> List.map (fun m -> m.ToByte())
            |> List.toArray

        let fixGarbled (text: string): string =
            let chars = text.ToCharArray()

            let existsGarbled =
                chars
                |> Array.exists(fun m -> int m = 65533)
            
            match existsGarbled with 
            | true -> Encoding.ANSICodePage.Value.GetString(bytes)
            | false -> text

        let readableName =  
            System.Text.Encoding.UTF8.GetString(bytes)
            |> fixGarbled


        member x.ReadableName = readableName

        member x.Parts = parts

        member x.RawName = v

        static member Create(v: string) =
            //v.Replace("#23", "#")
            DecodedPdfName v
    
    [<RequireQualifiedAccess>]
    type EncodedPdfNamePart =   
        | LetterOrDigit of char
        | Space 
        | Symbol of char
    with 
        static member Parse(char: char) =
            match char with 
            | If Char.IsDigit -> EncodedPdfNamePart.LetterOrDigit(char)
            | If Char.isEnglishLetter -> EncodedPdfNamePart.LetterOrDigit(char)
            | ' ' -> EncodedPdfNamePart.Space
            | char -> EncodedPdfNamePart.Symbol char
        
        member x.EncodedText =
            match x with 
            | LetterOrDigit v -> String(v, 1)
            | Space -> "#20"
            | Symbol char ->
                Encoding.UTF8.GetBytes([|char|])
                |> Array.map(fun m -> "#" + m.ToString("X2"))
                |> String.concat ""
    
    /// encoding
    type EncodedPdfName(v) =
        inherit POCOBaseV<string>(v)
        let __checkEncodedPdfNameValid =
            match v.TryReplaceStarting("/", "") with 
            | Some v2 -> failwithf "An decoded PdfName %s should not starts with \/" v2
            | None -> ()
        
        //let __checkNonSharpOperator =
        //    if v.Contains "#"
        //    then failwithf "An decoded PdfName %s should not contains #" v
        //    else ()

        let parts =
            v.ToCharArray()
            |> Array.map EncodedPdfNamePart.Parse
            
        let rawName = 
            parts
            |> Array.map(fun m -> m.EncodedText)
            |> String.concat ""
    
        let rawPdfName =
            let bytes = 
                rawName.ToCharArray()
                |> Array.map(System.Convert.ToByte)

            PdfName bytes

        member x.RawName = "/" + rawName
        member x.RawPdfName = rawPdfName
    
        member x.ReadableName = v

    [<Struct>]
    type OperatorRange =
        { Operator: PdfLiteral 
          Operands: IList<PdfObject> }

    type BlendMode =
        | Normal = 0
        | Multiply = 1
        | Screen = 2
        | Overlay = 3
        | Darken = 4
        | Lighten = 5
        | ColorDodge = 6
        | ColorBurn = 7
        | HardLight = 8
        | SoftLight = 9
        | Difference = 10
        | Exclusion = 11
        | Hue = 12
        | Saturation = 13
        | Color = 14
        | Luminosity = 15
        
    [<RequireQualifiedAccess>]
    module BlendMode =
        let toPdfName (blendMode: BlendMode) =
            PdfName(blendMode.ToString())

        let ofPdfName (pdfName: PdfName) =
            pdfName.ToString().TrimStart('/')
            |> stringToEnum<BlendMode>

    type ColorSpace =
        | Gray = 0
        | Rgb = 1
        | Cmyk = 2
        | Lab = 3

    type PdfLiteral with 
        member x.Text() = x.ToString()

    let private shortFontName (fontName: string) =
        let plusCount = 
            fontName.ToCharArray()
            |> Array.filter (fun m -> m = '+')
            |> Array.length

        match plusCount with 
        | 0 -> fontName
        | 1 -> fontName.RightOf("+").Value
        | _ -> failwithf "Multiple '+' exists in %s" fontName


    type FsFontName [<JsonConstructor>] (fontName: string) =
        inherit POCOBase<StringIC>(fontName |> shortFontName |> StringIC)
    
        let shortFontName = shortFontName fontName
            
        [<JsonProperty>]
        member private x.FontName = fontName
    
        member x.ShortFontName = shortFontName
    
        member x.LoggingText = shortFontName
    
        static member TryCreate (fontNames: FontNames) =
            let fontName = fontNames.GetFontName()
            match fontName with 
            | null -> None
            | fontName ->
                FsFontName(fontName)
                |> Some
    

    [<RequireQualifiedAccess>]
    type DocumentFontName = 
        | Valid of FsFontName
        | Invalid
    with    
        member x.SameFontNameTo(fontName: string) =
            match x with 
            | DocumentFontName.Invalid -> false
            | DocumentFontName.Valid x ->
                FsFontName(fontName).ShortFontName = x.ShortFontName



    [<StructuredFormatDisplay("{LoggingText}")>]
    type FsRectangle =
        { X: float 
          Y: float 
          Width: float 
          Height: float }
    with 
        member x.AsRectangle =
            Rectangle
                ( float32 x.X, 
                  float32 x.Y,
                  float32 x.Width,
                  float32 x.Height )

        member x.Left = x.X

        member x.Bottom = x.Y

        member x.Right = x.Left + x.Width

        member x.Top = x.Bottom + x.Height



        static member OfRectangle(rect: Rectangle) =
            { X      = rect.GetX()       |> float
              Y      = rect.GetY()       |> float
              Width  = rect.GetWidth()   |> float
              Height = rect.GetHeight()  |> float
            }

        member private x.MMValue =
            {| X = userUnitToMM x.X
               Y = userUnitToMM x.Y 
               Width = userUnitToMM x.Width
               Height = userUnitToMM x.Height |}

        member x.LoggingText = 
            let x = x.MMValue
            sprintf "RectMM %.1f %.1f %.1f %.1f" x.X x.Y x.Width x.Height

        static member create x y width height =
            { X = x 
              Y = y 
              Width = width
              Height = height }

        override x.ToString() = x.LoggingText

    [<RequireQualifiedAccess>]
    module Subpath =
        let toRawPoints (subpath: Subpath) =
            subpath.GetPiecewiseLinearApproximation()

    [<RequireQualifiedAccess>]
    module Rectangle =
        // <param name="points" at least two unique points></param>
        let ofPoints (points: Point al2List) =
            let xs,ys = 
                points.AsList 
                |> List.ofSeq
                |> List.map (fun p -> p.x,p.y) 
                |> List.unzip

            let x = List.min xs
            let y = List.min ys
            let width = List.max xs - x 
            let height = List.max ys - y
            Rectangle(float32 x, float32 y, float32 width, float32 height)
    
    type FsOPM =
        | Standard = 0
        | Illustractor = 1

    type ExtGStateAppereance =
        { IsOverprint: bool 
          Opacity: float32 }
    with 
        static member DefaultValue =
            { IsOverprint = false
              Opacity = 1.0f }

    type FsExtGState =
        { 
            OPM: FsOPM
            Fill: ExtGStateAppereance
            Stroke: ExtGStateAppereance
            BlendModes: BlendMode list
        }
    with 
        member x.IsFillOverprint = x.Fill.IsOverprint
        member x.IsStrokeOverprint = x.Stroke.IsOverprint

        member x.MapFill(f) =
            { x with Fill = f x.Fill }

        member x.MapStroke(f) =
            { x with Stroke = f x.Stroke }

        member x.SetStrokeIsOverprint(isOverprint) =
            x.MapStroke(fun m -> 
                { m with IsOverprint = isOverprint }
            )

        member x.SetStrokeOpacity(opacity) =
            x.MapStroke(fun m -> 
                { m with Opacity = opacity }
            )

        member x.SetFillIsOverprint(isOverprint) =
            x.MapFill(fun m -> 
                { m with IsOverprint = isOverprint }
            )

        member x.SetFillOpacity(opacity) =
            x.MapFill(fun m -> 
                { m with Opacity = opacity }
            )

        member x.MapAppereance(f) =
            { x with 
                Stroke = f x.Stroke
                Fill = f x.Fill}


        static member DefaultValue =
            { OPM = FsOPM.Illustractor 
              Fill = ExtGStateAppereance.DefaultValue
              Stroke = ExtGStateAppereance.DefaultValue
              BlendModes = [] }

        static member StrokeOverprint =
            FsExtGState.DefaultValue.MapStroke(fun m ->
                { m with IsOverprint = true }
            )

        static member FillOverprint =
            FsExtGState.DefaultValue.MapFill(fun m ->
                { m with IsOverprint = true }
            )

        static member FillStrokeOverprint =
            FsExtGState.DefaultValue.MapAppereance(fun m ->
                { m with IsOverprint = true }
            )

        static member FillTransparent(opacity) =
            FsExtGState.DefaultValue.MapFill(fun m ->
                { m with Opacity = opacity }
            )


        static member Transparent(opacity) =
            FsExtGState.DefaultValue.MapAppereance(fun m ->
                { m with Opacity = opacity }
            )


            

    type FsPoint =
        { X: float 
          Y: float }
    with 
        member private x.MMValue =
            {| X = userUnitToMM x.X
               Y = userUnitToMM x.Y |}

        static member Zero =
            { X = 0. 
              Y = 0. }

        static member OfPoint(point: Point) =
            { X = point.x 
              Y = point.y }

        member x.AsPoint = Point(x.X, x.Y)

    type Direction =
        | Vertical = 0
        | Horizontal = 1

    type BoundGettingStrokeOptions =
        | WithStrokeWidth = 0
        | WithoutStrokeWidth = 1


    type FillOrStrokeOptions =
        | Stroke = 0
        | Fill = 1
        | FillOrStroke = 2
        | FillAndStroke = 3

    type PageNumber(v) =
        inherit POCOBaseV<int>(v)

        let __checkPageNumberValid =
            match v > 0 with 
            | true -> ()
            | false -> failwithf "Cannot create pageNumber by %d" v

        member x.Value = v

    type TotalNumberOfPages(v) =
        inherit POCOBaseV<int>(v)

        let __checkPageNumberValid =
            match v > 0 with 
            | true -> ()
            | false -> failwithf "Cannot create pageNumber by %d" v

        member x.Value = v


    type IAbstractRenderInfo =
        abstract member Value: AbstractRenderInfo

    type IAbstractRenderInfoIM =
        abstract member Value: AbstractRenderInfo
        

    type IPathRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: PathRenderInfo
            
    type EndTextState =
        | Undified = 0
        | No = 1
        | Yes = 2

    type FollowedWord =
        { Space: float option 
          Text: string }

    type PdfConcatedText =
        { HeadWord: string
          FollowedWords: FollowedWord list }
    with 
        member x.AsList = x.HeadWord :: List.map(fun m -> m.Text) x.FollowedWords

        member x.ConcatedText(?wordSep) =
            x.AsList
            |> String.concat (defaultArg wordSep "")

        member x.TotalSpace =
            x.FollowedWords
            |> List.choose(fun m -> m.Space)
            |> List.sum

        member private x.AsFollowedTexts() =
            { Space = None; Text = x.HeadWord } :: x.FollowedWords

        static member (+) (a: PdfConcatedText, b: PdfConcatedText) =
            { HeadWord = a.HeadWord 
              FollowedWords = a.FollowedWords @ b.AsFollowedTexts()
            }

        member x.SplitToLines() =
            let words = x.AsFollowedTexts()
            
            let rec loop accum (words: FollowedWord list) =
                match words with 
                | [] -> List.rev accum
                | word :: t ->
                    let index = 
                        words
                        |> List.tryFindIndex(fun m -> m.Text.SplitToLines().Length > 1)

                    match index with 
                    | None -> loop ({ HeadWord = word.Text; FollowedWords = t } :: accum) []
                    | Some index ->
                        let left, right = List.splitAt index words 
                        let rightOne = right.[0]
                        let rightOneLines = rightOne.Text.SplitToLines()
                        let item  =
                            { HeadWord = word.Text 
                              FollowedWords = left @ [{Space = rightOne.Space; Text = rightOneLines.[0]}] }

                        let rightOneOtherText = 
                            { Space = None;
                              Text = rightOneLines.[1..] |> String.concat "\r\n" }

                        loop (item :: accum) (rightOneOtherText :: right.[1..])

            loop [] words

        static member Create(text, ?followedTexts) =
            { HeadWord = text 
              FollowedWords = defaultArg followedTexts [] }

    [<Struct>]
    type FollowedWordInfo =
        { Space: float option 
          TextInfo: TextRenderInfo }

    [<Struct>]
    type ConcatedTextInfo =
        { HeadWordInfo: TextRenderInfo
          FollowedWordInfos: FollowedWordInfo list }
    with 
        member x.AsList = x.HeadWordInfo :: List.map(fun m -> m.TextInfo) x.FollowedWordInfos

        member x.ConcatedText(?wordSep) =
            x.AsList
            |> List.map(fun m -> m.GetText())
            |> String.concat (defaultArg wordSep "")

        member x.PdfConcatedText() =
            { HeadWord = x.HeadWordInfo.GetText() 
              FollowedWords = 
                x.FollowedWordInfos
                |> List.map(fun m -> {Space = m.Space; Text = m.TextInfo.GetText()})
            }

    type ITextRenderInfo =
        inherit IAbstractRenderInfo
        abstract member Value: TextRenderInfo
        abstract member EndTextState: EndTextState
        abstract member ConcatedTextInfo: ConcatedTextInfo

    type IImageRenderInfo =
        inherit IAbstractRenderInfoIM
        abstract member Value: ImageRenderInfo

    type IntegratedRenderInfoTag =
        | Path = 0
        | Text = 1

    type IntegratedRenderInfoTagIM =
        | Path = 0
        | Text = 1
        | Image = 2

    [<RequireQualifiedAccess>]
    module IntegratedRenderInfoTagIM =
        let asIntegratedRenderInfoTag = function
            | IntegratedRenderInfoTagIM.Path -> IntegratedRenderInfoTag.Path |> Some
            | IntegratedRenderInfoTagIM.Text -> IntegratedRenderInfoTag.Text |> Some
            | IntegratedRenderInfoTagIM.Image -> None

    [<RequireQualifiedAccess>]
    type SerializableXObjectClippingBoxState =
        | Init 
        | IntersectedSome of FsRectangle
        | IntersectedNone 

    [<RequireQualifiedAccess; Struct>]
    type XObjectClippingBoxState =
        | Init 
        | IntersectedSome of Rectangle
        | IntersectedNone 
    with 
        member x.Serializable =
            match x with 
            | XObjectClippingBoxState.Init -> SerializableXObjectClippingBoxState.Init
            | XObjectClippingBoxState.IntersectedNone -> SerializableXObjectClippingBoxState.IntersectedNone
            | XObjectClippingBoxState.IntersectedSome v -> 
                SerializableXObjectClippingBoxState.IntersectedSome  (FsRectangle.OfRectangle v)

            

    
    [<Struct; RequireQualifiedAccess>]
    type ClippingPathInfoResult =
        | IntersectedSome of Rectangle
        | IntersectedNone 

    [<RequireQualifiedAccess>]
    module ClippingPathInfo =
        let private getActualClippingPath (info: ClippingPathInfo) = 
            match info.GetClippingPath() with 
            | null -> failwith "Not implemented"
            | path -> path
    
        let getActualClippingArea (info) =
            let clippingPath = getActualClippingPath info
            let points = 
                clippingPath.GetSubpaths()
                |> Seq.collect(Subpath.toRawPoints)
                |> List.ofSeq

            match points with 
            | [] -> ClippingPathInfoResult.IntersectedNone
            | _ -> Rectangle.ofPoints (AtLeastTwoList.Create points) |> ClippingPathInfoResult.IntersectedSome


    type IntersectedClippingPathInfoElement =
        { OperatorRanges: ResizeArray<OperatorRange> 
          Ctm: Matrix }

    type IntersectedClippingPathInfo =
        { ClippingPathInfo: ClippingPathInfo 
          Elements: array<ResizeArray<IntersectedClippingPathInfoElement>>  }
    
    [<RequireQualifiedAccess; Struct>]
    type ClippingPathInfoState =
        | Init 
        | Intersected of IntersectedClippingPathInfo
    with 
        member x.ActualClippingPathArea =
            match x with 
            | ClippingPathInfoState.Init _ -> None
            | ClippingPathInfoState.Intersected (v) -> Some (ClippingPathInfo.getActualClippingArea v.ClippingPathInfo)



    [<Struct>]
    type ClippingPathInfos =
        { XObjectClippingBoxState: XObjectClippingBoxState 
          ClippingPathInfoState: ClippingPathInfoState }


    type IIntegratedRenderInfoIM =
        inherit IAbstractRenderInfoIM
        abstract member TagIM: IntegratedRenderInfoTagIM
        abstract member ClippingPathInfos: ClippingPathInfos


    type IIntegratedRenderInfo =
        inherit IIntegratedRenderInfoIM
        inherit IAbstractRenderInfo
        abstract member Tag: IntegratedRenderInfoTag
        abstract member ClippingPathInfos: ClippingPathInfos





    type PageEdge =
        { LeftMiddle: Rectangle 
          LeftBottom: Rectangle 
          LeftTop: Rectangle 
          TopMiddle: Rectangle 
          TopRight: Rectangle 
          RightMiddle: Rectangle 
          RightBottom: Rectangle 
          BottomMiddle: Rectangle }


    [<RequireQualifiedAccess>]
    module PageEdge =
        let getAreas pageEdge =
            [ pageEdge.LeftMiddle
              pageEdge.LeftTop
              pageEdge.LeftBottom
              pageEdge.TopMiddle
              pageEdge.TopRight
              pageEdge.RightMiddle
              pageEdge.RightBottom
              pageEdge.BottomMiddle ]

    type Margin = 
        { Left: float
          Top: float
          Right: float
          Bottom: float }
    with   
        static member Create(value) =
            { Left = value 
              Top = value 
              Right = value 
              Bottom = value }

        static member Create(left, top, right, bottom) =
            { Left = left 
              Top = top 
              Right = right 
              Bottom = bottom }

        static member Create(values: float list) =
            match values with 
            | [ left; top; right; bottom ] ->
                { Left = left
                  Top = top
                  Right = right
                  Bottom = bottom }

            | _ -> failwithf "values' length %d is not equal to 4" values.Length

        static member Zero = Margin.Create(0.)

        static member MM6 = Margin.Create(mm 6.)
        static member MM3 = Margin.Create(mm 3.)

        static member ``MM1.5`` = Margin.Create(mm 1.5)


        static member (~-)(margin: Margin) =
            let f (v: float) = -v
            { Left = f margin.Left
              Top = f margin.Top
              Bottom = f margin.Bottom
              Right = f margin.Right
            }

        member x.LoggingText = 
            let distincted =
                [x.Left; x.Top; x.Right; x.Bottom]
                |> List.distinct

            match distincted with 
            | [one] -> sprintf "Margin %.1f" one
            | _ -> sprintf "Margin %.1f %.1f %.1f %.1f" x.Left x.Top x.Right x.Bottom


    [<RequireQualifiedAccess>]
    module Margin =
        let getValues (margin: Margin) =
            [ margin.Left; margin.Top; margin.Right; margin.Bottom]

        let mapValues f (margin: Margin) =
            { Left = f margin.Left
              Top = f margin.Top
              Bottom = f margin.Bottom
              Right = f margin.Right
            }

        let mapValuesByDirection direction f (margin: Margin) =
            match direction with 
            | Direction.Horizontal ->
                { margin with 
                    Left = f margin.Left
                    Right = f margin.Right
                }

            | Direction.Vertical ->
                { margin with 
                    Top =    f margin.Top
                    Bottom = f margin.Bottom
                }

      
    type PositiveMargin = private PositiveMargin of Margin 
    with 
        member x.Value =
            let (PositiveMargin v) = x
            v

        static member Create(margin: Margin) =
            margin
            |> Margin.mapValues(fun m -> (abs m))
            |> PositiveMargin

    type NegativeMargin = private NegativeMargin of Margin 
    with 
        member x.Value =
            let (NegativeMargin v) = x
            v

        static member Create(margin: Margin) =
            margin
            |> Margin.mapValues(fun m -> -(abs m))
            |> NegativeMargin

    type TileTableIndexer = 
        private TileTableIndexer of colNum: int * rowNum: int * hSpacing: float list * vSpacing: float list
    with 
        member x.ColNum =
            let (TileTableIndexer (colNum, rowNum, hSpacing, vSpacing)) = x
            colNum

        member x.RowNum = 
            let (TileTableIndexer (colNum, rowNum, hSpacing, vSpacing)) = x
            rowNum

        member x.HSpacing = 
            let (TileTableIndexer (colNum, rowNum, hSpacing, vSpacing)) = x
            hSpacing

        member x.VSpacing = 
            let (TileTableIndexer (colNum, rowNum, hSpacing, vSpacing)) = x
            vSpacing

        static member Create(colNum, rowNum, ?HSpacing, ?VSpacing) =
            if (colNum > 0 && rowNum > 0) then ()
            else failwithf "colNum %d and rowNum %d should bigger than 0" colNum rowNum 

            TileTableIndexer(colNum, rowNum, defaultArg HSpacing [0.], defaultArg VSpacing [0.])

    type TileCellIndexer = private TileCellIndexer of Direction * index: int
    with 

        member x.Index = 
            let (TileCellIndexer (direction, index)) = x
            index

        member x.Direction = 
            let (TileCellIndexer (direction, index)) = x
            direction

        static member Create(index, ?direction) =
            if index < 0 then failwithf "pieceIndex %d should >= 0" index
            TileCellIndexer(defaultArg direction Direction.Horizontal, index)

    type TileTable<'T> =
        private
            { 
              [<JsonProperty>]
              TileTableIndexer: TileTableIndexer
              [<JsonProperty>]
              Values: 'T [,] }

    with 
        member x.ColNum = x.TileTableIndexer.ColNum

        member x.RowNum = x.TileTableIndexer.RowNum

        member x.Item(index: TileCellIndexer) =
            match index.Direction with
            | Direction.Horizontal ->
                let index = index.Index
                let colIndex = index % x.ColNum
                let rowIndex = index / x.ColNum 
                x.Values.[rowIndex, colIndex]

            | Direction.Vertical ->
                let index = index.Index
                let rowIndex = index % x.RowNum
                let colIndex = (index / x.RowNum) 
                x.Values.[rowIndex, colIndex]


        member x.Item(index: int) =
            let index = TileCellIndexer.Create(index)
            x.Item(index)

        static member Create(tileTable: TileTableIndexer, values: _ [,]) =
            let cellsCount = tileTable.ColNum * (tileTable.RowNum)
            match values.Length with 
            | EqualTo cellsCount -> 
                { TileTableIndexer = tileTable
                  Values = values }
                
            | length -> failwithf "Cannot create tileTable collection from (tileTable: %A, values length: %d)" (tileTable) length


    type AreaRow = AreaRow of FsRectangle list
    with 
        member x.Length = 
            let (AreaRow v) = x
            v.Length

        member x.Cells =
            let (AreaRow v) = x
            v

    //type PageAreas = PageAreas of FsRectangle list
    //with 
    //    member x.Areas =
    //        let (PageAreas v) = x
    //        v

    //    member x.AreasCount = x.Areas.Length

    type AreaTable = private AreaTable of AreaRow list
    with 
        member x.Rows =
            let (AreaTable (v)) = x
            v

        member x.Cells =
            x.Rows
            |> List.collect(fun m -> m.Cells)

        //member x.AsPageAreas =
        //    x.Cells
        //    |> PageAreas

        member x.CellsCount = 
            x.Rows
            |> List.sumBy(fun m -> m.Length)

        //member x.ColNum =
        //    let (AreaTable (v, _)) = x
        //    v

        static member Create(rows: AreaRow list) =
            //let colNum = 
            //    rows
            //    |> AtLeastOneList.map(fun m -> m.Length)
            //    |> AtLeastOneList.exactlyOne_DetailFailingText

            AreaTable(rows)
            

        static member Create(rows: FsRectangle list list) =
            //let colNum = 
            //    rows
            //    |> AtLeastOneList.map(fun m -> m.Length)
            //    |> AtLeastOneList.exactlyOne_DetailFailingText
            rows
            |> List.map(AreaRow)
            |> AreaTable
    
    type Flip =
        | HFlip = 0
        | VFlip = 1

    [<RequireQualifiedAccess>]
    type Rotation =
        | None = 0
        | Clockwise = 1
        | Counterclockwise = 2
        | R180 = 3

    type Margin with 
        member x.Rotate(rotation: Rotation) =
            match rotation with
            | Rotation.Clockwise ->
                Margin.Create(x.Bottom, x.Left, x.Top, x.Right)
            | Rotation.Counterclockwise ->
                Margin.Create(x.Top, x.Right, x.Bottom, x.Left)

            | Rotation.None -> x
            | Rotation.R180 -> Margin.Create(x.Right, x.Bottom, x.Left, x.Top) 


    [<RequireQualifiedAccess>]
    module Rotation =
        let toDirection = function
            | Rotation.None
            | Rotation.R180 -> Direction.Horizontal
            | Rotation.Clockwise
            | Rotation.Counterclockwise -> Direction.Vertical

        let ofCharOp(char: char) =
            match char with 
            | '>' -> Some Rotation.Clockwise
            | '<' -> Some Rotation.Counterclockwise
            | '*' -> Some Rotation.R180
            | _ -> None


        let ofChar(char: char) =
            match ofCharOp char with 
            | Some rotation -> rotation
            | None -> failwithf "Cannot create rotation from %A, avaliable chars are %A" char [">"; "<"; "*"]



        let toChar = function
            | Rotation.Clockwise  -> Some '>'
            | Rotation.Counterclockwise -> Some '<'
            | Rotation.R180 -> Some '*'
            | Rotation.None -> None


        let getAngle = function
            | Rotation.Clockwise  -> 90.
            | Rotation.Counterclockwise -> -90.
            | Rotation.R180 -> 180.
            | Rotation.None -> 0.

        let ofAngle angle = 
            let angle = 
                match angle with 
                | SmallerThan 0. -> (abs angle) + 180.
                | _ -> angle

            match (angle) % 360. with
            | 90. -> Rotation.Clockwise
            | 180. -> Rotation.R180
            | 0. -> Rotation.None
            | 270. -> Rotation.Counterclockwise
            | v -> failwithf "Cannot create rotation by angle %A" angle

        let concatenate (rotation1) (rotation2) =
            getAngle rotation1 + getAngle rotation2
            |> ofAngle


        let getRadians rotation = 
            let angle = getAngle rotation
            (System.Math.PI / 180.) * angle
            


        let isNon = function
            | Rotation.None  -> true
            | _ -> false

        let notNon = function
            | Rotation.None  -> false
            | _ -> true


    type AffineTransformRecord =
        { ScaleX: float 
          ShearX: float 
          ShearY: float
          ScaleY: float 
          TranslateX: float 
          TranslateY: float }
    with
        member x.m00 = x.ScaleX

        member x.m01 = x.ShearX

        member x.m10 = x.ShearY

        member x.m11 = x.ScaleY

        member x.m02 = x.TranslateX

        member x.m12 = x.TranslateY

        static member GetFlipInstance(flip: Flip) =
            match flip with 
            | Flip.HFlip -> 
                { ScaleX = -1.0
                  ShearX = 0.0 
                  ShearY = 0.0 
                  ScaleY = 1.0
                  TranslateX = 0.0
                  TranslateY = 0.0 }
                
            | Flip.VFlip ->
                { ScaleX = 1.0
                  ShearX = 0.0 
                  ShearY = 0.0 
                  ScaleY = -1.0
                  TranslateX = 0.0
                  TranslateY = 0.0 }
                

        member x.MapValue(fValue) =
            {
                ScaleX      = fValue x.ScaleX
                ShearX      = fValue x.ShearX
                ShearY      = fValue x.ShearY
                ScaleY      = fValue x.ScaleY
                TranslateX  = fValue x.TranslateX
                TranslateY  = fValue x.TranslateY   
            }

        
        static member DefaultValue =
            { ScaleX = 1. 
              ScaleY = 1. 
              TranslateX = 0.
              TranslateY = 0.
              ShearX = 0.
              ShearY = 0. }

            

    module AffineTransformRecord =
        let ofAffineTransform (affineTransform: AffineTransform) =
            { ScaleX = affineTransform.GetScaleX() 
              ShearX = affineTransform.GetShearX() 
              ShearY = affineTransform.GetShearY() 
              ScaleY = affineTransform.GetScaleY() 
              TranslateX = affineTransform.GetTranslateX() 
              TranslateY = affineTransform.GetTranslateY() }

        let toAffineTransform (record: AffineTransformRecord) =
            new AffineTransform(
                record.m00,
                record.m10,
                record.m01,
                record.m11,
                record.m02,
                record.m12
            )



        let ofMatrix (matrix: Matrix) =
            let values =
                [| matrix.Get(Matrix.I11)
                   matrix.Get(Matrix.I12)
                   matrix.Get(Matrix.I21)
                   matrix.Get(Matrix.I22)
                   matrix.Get(Matrix.I31)
                   matrix.Get(Matrix.I32) |]
            
            values
            |> AffineTransform
            |> ofAffineTransform
     

        let toMatrix (record: AffineTransformRecord) =
            let values = Array.create 6 0.f
            (toAffineTransform record).GetMatrix(values)
            
            new Matrix(values.[0], values.[1], values.[2], values.[3], values.[4], values.[5])
            //new Matrix(values.[Matrix.I11], values.[Matrix.I12], values.[Matrix.I21], values.[Matrix.I22], values.[Matrix.I31], values.[Matrix.I32])

    type AffineTransformRecord with 
        member x.Concatenate(y: AffineTransformRecord) =
            let x = AffineTransformRecord.toAffineTransform x
            (x).Concatenate(AffineTransformRecord.toAffineTransform y)
            AffineTransformRecord.ofAffineTransform x

        member x.PreConcatenate(y: AffineTransformRecord) =
            let x = AffineTransformRecord.toAffineTransform x
            (x).PreConcatenate(AffineTransformRecord.toAffineTransform y)
            AffineTransformRecord.ofAffineTransform x

        member x.Inverse() =
            let x = AffineTransformRecord.toAffineTransform x
            x.CreateInverse()
            |> AffineTransformRecord.ofAffineTransform

    type DashPattern =
        { DashArray: float []
          Phase: float }

    with 
        member x.DashArrayF32 =
            x.DashArray
            |> Array.map float32

        member x.PhaseF32 =
            float32 x.Phase

        static member Create(value) =
            { DashArray = [|value|]
              Phase = value }

        member x.IsEmpty = x = DashPattern.Empty

        static member Empty = {DashArray = [||]; Phase = 0.}

        static member MM2 = DashPattern.Create(mm 2.)

    type PageBoxKind =
        | ArtBox = 0
        | BleedBox = 1
        | TrimBox = 2
        | CropBox = 3
        | MediaBox = 4
        | ActualBox = 5
        | AllBox = 6


    [<RequireQualifiedAccess>]
    type AreaGettingOptions =
        | PageBox of PageBoxKind
        | PageBoxWithOffset of PageBoxKind * Margin
        | FsSpecfic of FsRectangle
    with    
        static member Specfic(rect) = 
            AreaGettingOptions.FsSpecfic(FsRectangle.OfRectangle rect)



    [<RequireQualifiedAccess>]
    type CanvasFontSize =
        | Numeric of size: float
        | OfRootArea of scale: float
        | OfFsArea of FsRectangle * scale: float
    with    
        static member OfArea(rect, ?scale) =
            CanvasFontSize.OfFsArea(FsRectangle.OfRectangle rect, defaultArg scale 1.)

    [<RequireQualifiedAccess>]
    type RelativePosition =
        | Inbox = 0
        | CrossBox = 1
        | OutBox = 2



    type StraightLine =
        { Start: Point
          End: Point }


    type YEffort =
        | Top = 0
        | Bottom  = 1
        | Middle  = 2

    type XEffort =
        | Left = 0
        | Right = 1
        | Middle  = 2

    type Effort =
        { XEffort: XEffort 
          YEffort: YEffort }
    with 
        static member Center = 
            { XEffort = XEffort.Middle 
              YEffort = YEffort.Middle }

    type PositionEnum =
        | LeftBottom          =    0
        | LeftMiddle          =    1
        | LeftTop             =    2
        | TopMiddle           =    3
        | RightTop            =    4
        | RightMiddle         =    5
        | RightBottom         =    6
        | BottomMiddle        =    7
        | Center              =    8

    [<RequireQualifiedAccess>]
    type Position =
        | LeftBottom of float * float
        | LeftMiddle of float * float
        | LeftTop of float * float
        | TopMiddle of float * float
        | RightTop of float * float
        | RightMiddle of float * float
        | RightBottom of float * float
        | BottomMiddle of float * float
        | Center of float * float




    [<RequireQualifiedAccess>]
    module Position =
        let create x y = function
            | PositionEnum.LeftBottom          ->   Position.LeftBottom      (x, y)
            | PositionEnum.LeftMiddle          ->   Position.LeftMiddle      (x, y)
            | PositionEnum.LeftTop             ->   Position.LeftTop         (x, y)
            | PositionEnum.TopMiddle           ->   Position.TopMiddle       (x, y)
            | PositionEnum.RightTop            ->   Position.RightTop        (x, y)
            | PositionEnum.RightMiddle         ->   Position.RightMiddle     (x, y)
            | PositionEnum.RightBottom         ->   Position.RightBottom     (x, y)
            | PositionEnum.BottomMiddle        ->   Position.BottomMiddle    (x, y)
            | PositionEnum.Center              ->   Position.Center          (x, y)


        let (|Left|XCenter|Right|) = function
            | Position.LeftTop (x, y) 
            | Position.LeftBottom (x, y) 
            | Position.LeftMiddle(x, y) -> Left (x, y)
            | Position.Center (x, y) 
            | Position.BottomMiddle (x, y) 
            | Position.TopMiddle(x, y) -> XCenter (x, y)
            | Position.RightBottom (x, y) 
            | Position.RightTop (x, y)
            | Position.RightMiddle (x, y) -> Right (x, y)

        let (|Bottom|Top|YCenter|) = function
            | Position.LeftBottom (x, y)
            | Position.RightBottom (x, y)
            | Position.BottomMiddle (x, y)-> Bottom (x, y)
            | Position.LeftTop (x, y) 
            | Position.RightTop (x, y)
            | Position.TopMiddle(x, y) -> Top (x, y)
            | Position.Center (x, y) 
            | Position.LeftMiddle (x, y) 
            | Position.RightMiddle(x, y) -> YCenter (x, y)

        let (|LeftEdge|_|) = function
            | Left (0., _) -> Some ()
            | _ -> None
    
        let (|BottomEdge|_|) = function
            | Bottom (_, 0.) -> Some ()
            | _ -> None

        let (|TopEdge|_|) = function
            | Top (_, 0.) -> Some ()
            | _ -> None

        let (|RightEdge|_|) = function
            | Right (0., _)  -> Some ()
            | _ -> None

        let getValue position = 
            match position with
            | Position.LeftBottom (x, y)
            | Position.LeftMiddle (x, y)
            | Position.LeftTop (x, y) 
            | Position.TopMiddle (x, y) 
            | Position.RightTop (x, y)
            | Position.RightMiddle (x, y)
            | Position.RightBottom (x, y)
            | Position.BottomMiddle (x, y) 
            | Position.Center (x, y) -> x, y

        let mapValue (mapping) position = 
            match position with
            | Position.LeftBottom (x, y)-> Position.LeftBottom (mapping (x, y))
            | Position.LeftMiddle (x, y) -> Position.LeftMiddle (mapping (x, y))
            | Position.LeftTop (x, y) -> Position.LeftTop (mapping (x, y))
            | Position.TopMiddle (x, y) -> Position.TopMiddle (mapping (x, y))
            | Position.RightTop (x, y) -> Position.RightTop (mapping (x, y))
            | Position.RightMiddle (x, y) -> Position.RightMiddle (mapping (x, y))
            | Position.RightBottom (x, y) -> Position.RightBottom (mapping (x, y))
            | Position.BottomMiddle (x, y) -> Position.BottomMiddle (mapping (x, y))
            | Position.Center (x, y) -> Position.Center (mapping (x, y))

    type TextRenderingMode = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.TextRenderingMode
    type LineJoinStyle = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.LineJoinStyle
    type LineCapStyle = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.LineCapStyle
    type FillingRule = iText.Kernel.Pdf.Canvas.PdfCanvasConstants.FillingRule



    [<RequireQualifiedAccess>]
    type SinglePageSelectorExpr =
        | Begin of int
        | End of int

    with 
        member x.AsSingleIndexExpr =
            match x with 
            | SinglePageSelectorExpr.Begin v -> SingleIndexExpr.Begin v
            | SinglePageSelectorExpr.End v -> SingleIndexExpr.End v

        static member OfSingleIndexExpr(expr) =
            match expr with 
            | SingleIndexExpr.Begin v -> SinglePageSelectorExpr.Begin v
            | SingleIndexExpr.End v   -> SinglePageSelectorExpr.End v  

        override x.ToString() =
            match x with 
            | SinglePageSelectorExpr.Begin i -> i.ToString()
            | SinglePageSelectorExpr.End i -> "R" + i.ToString()

    [<RequireQualifiedAccess>]
    type PageSelectorExpr = 
        | SinglePage of SinglePageSelectorExpr
        | Between of SinglePageSelectorExpr * SinglePageSelectorExpr
        | Compose of PageSelectorExpr list

    with
        member x.AsIndexExpr =
            match x with 
            | PageSelectorExpr.SinglePage  v       ->  IndexExpr.Single   v.AsSingleIndexExpr       
            | PageSelectorExpr.Between    (v1, v2) ->  IndexExpr.Between     (v1.AsSingleIndexExpr, v2.AsSingleIndexExpr) 
            | PageSelectorExpr.Compose     v       ->  
                v       
                |> List.map(fun m -> m.AsIndexExpr)
                |> IndexExpr.Compose

        static member OfIndexExpr(expr) =
            match expr with 
            | IndexExpr.Single  v       ->  
                PageSelectorExpr.SinglePage  (SinglePageSelectorExpr.OfSingleIndexExpr v)
            | IndexExpr.Between    (v1, v2) ->  
                let v1 =  SinglePageSelectorExpr.OfSingleIndexExpr v1
                let v2 =  SinglePageSelectorExpr.OfSingleIndexExpr v2
                PageSelectorExpr.Between (v1, v2)
            | IndexExpr.Compose     v       ->  
                v       
                |> List.map(PageSelectorExpr.OfIndexExpr)
                |> PageSelectorExpr.Compose

        override x.ToString() =
            match x with 
            | PageSelectorExpr.SinglePage expr -> expr.ToString()
            | PageSelectorExpr.Between (expr1, expr2) -> expr1.ToString() + "-" + expr2.ToString()
            | Compose exprs ->
                exprs
                |> List.map (fun m -> m.ToString())
                |> String.concat ", "

    [<RequireQualifiedAccess>]
    module PageSelectorExpr = 
        let create (exprText: string) =
            IndexExpr.create exprText
            |> PageSelectorExpr.OfIndexExpr



    [<RequireQualifiedAccess>]
    type PageSelector =
        | Last
        | First
        | All
        | MultipleOf of int
        | Odd 
        | Even
        | Expr of PageSelectorExpr
        | Numbers of AtLeastOneSet<int>
        | And of PageSelector list
    with 
        override x.ToString() =
            match x with 
            | PageSelector.Last -> "1R"
            | PageSelector.First -> "1"
            | PageSelector.All -> "ALL"
            | PageSelector.MultipleOf multiple -> sprintf "X%d" multiple
            | PageSelector.Odd -> "Odd"
            | PageSelector.Even -> "Even"
            | PageSelector.And pageSelectors ->
                let listText = 
                    pageSelectors
                    |> List.map(fun pageSelector -> pageSelector.ToString())
                    |> String.concat "; "

                sprintf "AND [%s]" listText 

            | PageSelector.Expr expr -> expr.ToString()
            | PageSelector.Numbers numbers -> 
                let numbers = 
                    numbers.Value
                    |> Set.toList
                    |> List.map string
                    |> String.concat ", "

                let numbers = "[" + numbers + "]"

                if numbers.Length > 80
                then 
                    let chars = 
                        numbers.ToCharArray()
                        |> Array.take 80

                    let v = new System.String(chars)

                    v + ".... "

                else 
                    numbers

        member x.Text = x.ToString()

        static member Number(pageNumber: int) =
            AtLeastOneSet.Create [pageNumber]
            |> PageSelector.Numbers


    [<RequireQualifiedAccess>]
    type NullablePageSelector =
        | PageSelector of PageSelector
        //| Last
        //| First
        //| All
        //| Expr of PageSelectorExpr
        //| Numbers of AtLeastOneSet<int>
        | Non
    with 

        static member Last                = PageSelector(PageSelector.Last)     
        
        static member First               = PageSelector(PageSelector.First)      
        
        static member All                 = PageSelector(PageSelector.All)         
        
        static member Expr(expr)          = PageSelector(PageSelector.Expr(expr))       
        
        static member Numbers(numbers)    = PageSelector(PageSelector.Numbers(numbers))   
        
        static member And(pageSelectors)  = PageSelector(PageSelector.And(pageSelectors)) 
        
        static member Odd                 = PageSelector(PageSelector.Odd) 
        
        static member Even                = PageSelector(PageSelector.Even)       


        member x.AsPageSelector =
            match x with 
            //| NullablePageSelector.Last         ->   Some (PageSelector.Last     )
            //| NullablePageSelector.First        ->   Some (PageSelector.First    )
            //| NullablePageSelector.All          ->   Some (PageSelector.All      )
            //| NullablePageSelector.Expr    v    ->   Some (PageSelector.Expr v   )
            //| NullablePageSelector.Numbers v    ->   Some (PageSelector.Numbers v)
            | NullablePageSelector.PageSelector pageSelector -> Some pageSelector
            | NullablePageSelector.Non -> None
                                                                  
    type PdfDocument with

        member pdfDocument.GetPageNumber(pageSelectorExpr: SinglePageSelectorExpr) =
            let totalPageNum = pdfDocument.GetNumberOfPages()
            
            match pageSelectorExpr with
            | SinglePageSelectorExpr.Begin (i) -> i
            | SinglePageSelectorExpr.End (i) -> totalPageNum - i + 1


        member pdfDocument.GetPageNumbers(pageSelectorExpr: PageSelectorExpr) =

            match pageSelectorExpr with 
            | PageSelectorExpr.SinglePage singlePage -> [pdfDocument.GetPageNumber singlePage]

            | PageSelectorExpr.Between (beginExpr, endExpr) ->
                [pdfDocument.GetPageNumber beginExpr .. pdfDocument.GetPageNumber endExpr]

            | PageSelectorExpr.Compose compose ->
                compose
                |> List.collect (pdfDocument.GetPageNumbers)
                |> List.distinct

        member pdfDocument.GetPages() =
            [
                for i = 1 to pdfDocument.GetNumberOfPages() do
                    pdfDocument.GetPage(i)
            ]

        member pdfDocument.GetPages(pageSelectorExpr: PageSelectorExpr) =
            let numbers = pdfDocument.GetPageNumbers(pageSelectorExpr)
            numbers
            |> List.map(fun number ->
                pdfDocument.GetPage(number)
            )
  



        member pdfDocument.GetPageNumbers(pageSelector) =
            let numberOfPages = pdfDocument.GetNumberOfPages()
            match pageSelector with 
            | PageSelector.First -> [1]
            | PageSelector.Last -> [numberOfPages]
            | PageSelector.MultipleOf multiple -> 
                [1..numberOfPages]
                |> List.filter(fun m ->
                    m % multiple = 0
                )
            | PageSelector.Odd -> 
                [1..numberOfPages]
                |> List.filter(isOdd)

            | PageSelector.Even -> 
                [1..numberOfPages]
                |> List.filter(isEven)

            | PageSelector.Expr expr -> 
                pdfDocument.GetPageNumbers(expr)
            | PageSelector.All -> [1..numberOfPages]
            | PageSelector.Numbers numbers -> 
                let intersectedNumbers =
                    Set.intersect
                        numbers.Value
                        (Set.ofList [1..numberOfPages])
                    |> Set.toList

                intersectedNumbers

            | PageSelector.And (pageSelectors: PageSelector list) ->
                let pageNumberLists = 
                    pageSelectors
                    |> List.map(fun pageSelector ->
                        pdfDocument.GetPageNumbers(pageSelector)
                    )

                match pageNumberLists with 
                | [] -> []
                | _ ->
                    pageNumberLists
                    |> List.reduce(fun pageNumbers1 pageNumbers2 ->
                        pageNumbers1
                        |> List.filter(fun pageNumber1 ->
                            List.contains pageNumber1 pageNumbers2
                        )
                    )


        member pdfDocument.GetPages(pageSelector: PageSelector) =
            let numbers = pdfDocument.GetPageNumbers(pageSelector)
            numbers
            |> List.map(fun number ->
                pdfDocument.GetPage(number)
            )

        member pdfDocument.GetPageNumbers(pageSelector: NullablePageSelector) =
            match pageSelector.AsPageSelector with 
            | Some pageSelector -> pdfDocument.GetPageNumbers(pageSelector)
            | None -> []
       