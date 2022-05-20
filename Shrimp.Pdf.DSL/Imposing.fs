namespace Shrimp.Pdf


#nowarn "0104"
open iText.Kernel.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open Shrimp.Pdf.Extensions
open System.Linq
open System
open System.Collections.Concurrent
open Shrimp.FSharp.Plus
open Shrimp.FSharp.Plus.Math
open Newtonsoft.Json
open Shrimp.Pdf.Constants
open Fake.IO



type BackgroundFile = private BackgroundFile of originPdf: PdfFile * clearedPdf: PdfFile * currrentRotation: Rotation
with
    member private x.Value =
        let (BackgroundFile (value, _, _)) = x
        value

    member x.ClearedPdfFile =
        let (BackgroundFile (_, v, _)) = x
        v

    member internal x.CurrentRotation = 
        let (BackgroundFile (_, _, value)) = x
        value
        
    member x.Rotate(rotation) =
        let angle_plus = Rotation.getAngle rotation
        let newRotation =
            let angle = (Rotation.getAngle x.CurrentRotation)
            Rotation.ofAngle(angle + angle_plus)
            
        BackgroundFile(x.Value, x.ClearedPdfFile, newRotation)


    member x.Clockwise() = x.Rotate(Rotation.Clockwise)

    member x.CounterClockwise() = x.Rotate(Rotation.Counterclockwise)


    static member Create(pdfFile: PdfFile) =
        let targetPath =
            pdfFile.Path
            |> Path.changeExtension "backgroundFile.cleared.pdf"

        let clearedPdfFile = 
            if File.exists targetPath then PdfFile targetPath
            else 
                let flow = 
                    Reuses.ClearDirtyInfos()

                runWithBackup targetPath pdfFile.Path (Flow.Reuse flow)
                |> List.exactlyOne_DetailFailingText
                |> fun flowModel -> flowModel.PdfFile

        (pdfFile, clearedPdfFile, Rotation.None)
        |> BackgroundFile

    static member Create(path: string) =
        PdfFile path
        |> BackgroundFile.Create

[<RequireQualifiedAccess>]
module BackgroundFile =
    let private backgroundFilePageBoxCache = new ConcurrentDictionary<BackgroundFile, Rectangle list>()

    let getPageBoxes = 
        fun (backGroundFile: BackgroundFile) ->
            backgroundFilePageBoxCache.GetOrAdd(backGroundFile, fun backGroundFile ->
                let reader = new PdfDocument(new PdfReader(backGroundFile.ClearedPdfFile.Path))
                PdfDocument.getPages reader
                |> List.map (fun page ->
                    let pageBox = (page.GetActualBox())
                    reader.Close()
                    pageBox.RotateByCenter(backGroundFile.CurrentRotation)
                )
            )

    let getSize (backgroundFile) =
        getPageBoxes backgroundFile
        |> function
            | [pageBox] -> FsSize.ofRectangle pageBox
            | h :: t -> failwithf "Cannot get background size as more than 1 pages founded in file %A" backgroundFile
            | [] -> failwithf "Cannot get background size as no pages founded in file %A" backgroundFile

    let getPagebox (backgroundFile) =
         getPageBoxes backgroundFile
         |> function
             | [pageBox] -> pageBox
             | h :: t -> failwithf "Cannot get background size as more than 1 pages founded in file %A" backgroundFile
             | [] -> failwithf "Cannot get background size as no pages founded in file %A" backgroundFile



type RowNumber(v) =
    inherit POCOBaseV<int>(v)

    let __checkPageNumberValid =
        match v > 0 with 
        | true -> ()
        | false -> failwithf "Cannot create rowNumber by %d" v

    member x.Value = v

type ColumnNumber(v) =
    inherit POCOBaseV<int>(v)

    let __checkPageNumberValid =
        match v > 0 with 
        | true -> ()
        | false -> failwithf "Cannot create columnNumber by %d" v

    member x.Value = v

type RowOrColumn =
    | Row = 0
    | Column = 1

[<AutoOpen>]
module _RowOrColumnEx =
    type RowOrColumn with
        member x.Negative =
            match x with 
            | RowOrColumn.Row -> RowOrColumn.Column
            | RowOrColumn.Column -> RowOrColumn.Row

[<RequireQualifiedAccess>]
type RowOrColumnNumber =
    | RowNumber of RowNumber
    | ColumnNumber of ColumnNumber
with 
    member x.Value =
        match x with 
        | RowOrColumnNumber.RowNumber v -> v.Value
        | RowOrColumnNumber.ColumnNumber v -> v.Value

module Imposing =



    type Cropmark = 
        { Length: float
          Distance: float
          Width: float
          IsRevealedBetweenCells: bool
          Color: PdfCanvasColor }

    [<RequireQualifiedAccess>]
    module Cropmark =
        let defaultValue = 
            { Length = mm 3.8
              Distance = mm 3.2
              Width = mm 0.1
              IsRevealedBetweenCells = true
              Color = PdfCanvasColor.Registration }


    [<RequireQualifiedAccess>]
    type Sheet_PlaceTable =
        | Trim_CenterTable of Margin
        | At of Position






    [<RequireQualifiedAccess>]
    type Background =
        | Size of FsSize
        | File of BackgroundFile
    with 
        member x.Rotate(rotation: Rotation) =
            match x with 
            | Background.Size v -> FsSize.rotate rotation v |> Background.Size
            | Background.File v -> v.Rotate(rotation) |> Background.File

    [<RequireQualifiedAccess>]
    module Background =
        let getSize = function
            | Background.Size pageSize -> pageSize
            | Background.File backgroundFile -> BackgroundFile.getSize backgroundFile


    type NumericFillingMode =
        { ColNums: int list 
          RowNum: int }


    type ColumnAutomaticFillingMode =
        { RowNum: int }

    type RowAutomaticFillingMode =
        { ColNums: int list }

    [<RequireQualifiedAccess>]
    type FillingMode =
        | Numeric of NumericFillingMode
        | Automatic
        | ColumnAutomatic of ColumnAutomaticFillingMode
        | RowAutomatic of RowAutomaticFillingMode


    [<RequireQualifiedAccess>]
    module FillingMode =
        let (|ColNumsSpecific|_|) = function
            | FillingMode.Numeric numericFillingMode ->
                Some numericFillingMode.ColNums
            | FillingMode.RowAutomatic rowAutomaticFillingMode -> Some rowAutomaticFillingMode.ColNums
            | _ -> None


        let (|RowNumSpecific|_|) = function
            | FillingMode.Numeric numericFillingMode ->
                Some numericFillingMode.RowNum
            | FillingMode.ColumnAutomatic columnAutomaticFillingMode -> Some columnAutomaticFillingMode.RowNum
            | _ -> None




    type DesiredPageOrientation =
        | Landscape  = 0
        | Portrait = 1
        | Automatic = 2


    [<RequireQualifiedAccess>]
    module DesiredPageOrientation =

        let (|PageOrientation|_|) = function
            | DesiredPageOrientation.Landscape -> Some PageOrientation.Landscape
            | DesiredPageOrientation.Portrait -> Some PageOrientation.Portrait
            | DesiredPageOrientation.Automatic -> None

    [<RequireQualifiedAccess>]
    type CellRotation =
        | None
        | R180WhenColNumIsEven 
        | R180WhenRowNumIsEven 

    type SpaceMiddleLine =
        { Properties: PdfCanvasAddLineArguments 
          EdgeLength_PercentToMargin: float }
    with 
        static member DashLine(?dashValue, ?edgeLength_PercentToMargin) =
            { Properties =
                PdfCanvasAddLineArguments.DashLine(?value = dashValue)
              EdgeLength_PercentToMargin = defaultArg edgeLength_PercentToMargin 0.7
            }

        member x.SetProps(props) =
            { x with 
                Properties = props x.Properties }


        member x.SetWidth(width) =
            x.SetProps(fun m -> {m with LineWidth = width})

    type Space =
        { Space: float 
          MiddleLine: SpaceMiddleLine option }
    with 
        static member Zero =
            { Space = 0. 
              MiddleLine = None }

        member x.TrySetMiddleLineProps(props) =
            { x with 
                MiddleLine = 
                    match x.MiddleLine with 
                    | Some v ->     
                        { v with Properties = props v.Properties }
                        |> Some
                    | None -> None
            }

        static member Value(value) = { Space = value; MiddleLine = None }

        static member MiddleDashLine(value, ?dashValue, ?edgeLength_PercentToMargin) =
            { Space = value 
              MiddleLine = 
                Some (SpaceMiddleLine.DashLine(?dashValue = dashValue, ?edgeLength_PercentToMargin = edgeLength_PercentToMargin))
                    
                
              }

    type Spaces [<JsonConstructor>] (v) =
        inherit POCOBaseV<Space list>(v)

        member x.Value = v

        new (value: float) =
            Spaces 
                [
                    { Space = value
                      MiddleLine = None }
                ]

        new (values: float list) =
            Spaces 
                (values |> List.map(fun value -> {Space = value; MiddleLine = None}))


        new (space: Space) = Spaces [space]

        member x.TrySetMiddleLineProps(props) =
            v
            |> List.map(fun m -> m.TrySetMiddleLineProps(props))
            |> Spaces

        static member Zero = Spaces (Space.Zero)

        static member MiddleDashLine(value, ?dashValue) = Spaces(Space.MiddleDashLine(value, ?dashValue = dashValue))


    type _ImposingArguments =
        {
            ColNums: int list
            RowNum: int
            Cropmark: Cropmark option
            HSpaceExes: Spaces
            VSpaceExes: Spaces
            UseBleed: bool
            Background: Background
            DesiredPageOrientation: DesiredPageOrientation
            Sheet_PlaceTable: Sheet_PlaceTable
            DesiredSizeOp: FsSize option
            CellRotation: CellRotation
            IsRepeated: bool
        }

    with 
        member x.HSpaces =
            x.HSpaceExes.Value
            |> List.map(fun m -> m.Space)

        member x.VSpaces =
            x.VSpaceExes.Value
            |> List.map(fun m -> m.Space)

        member internal args.Rotate(rotation: Rotation) =
            { args with 
                DesiredSizeOp =
                    match args.DesiredSizeOp with 
                    | Some size -> FsSize.rotate rotation size |> Some
                    | None -> None
                HSpaceExes = 
                    match rotation with 
                    | Rotation.None
                    | Rotation.R180 -> args.HSpaceExes
                    | Rotation.Clockwise
                    | Rotation.Counterclockwise -> args.VSpaceExes
                VSpaceExes = 
                    match rotation with 
                    | Rotation.None
                    | Rotation.R180 -> args.VSpaceExes
                    | Rotation.Clockwise
                    | Rotation.Counterclockwise -> args.HSpaceExes

                DesiredPageOrientation =
                    match rotation with 
                    | Rotation.None
                    | Rotation.R180 -> args.DesiredPageOrientation
                    | _ ->
                        match args.DesiredPageOrientation with 
                        | DesiredPageOrientation.Automatic -> DesiredPageOrientation.Automatic
                        | DesiredPageOrientation.Landscape -> DesiredPageOrientation.Portrait
                        | DesiredPageOrientation.Portrait -> DesiredPageOrientation.Landscape
                Sheet_PlaceTable = 
                    match args.Sheet_PlaceTable with 
                    | Sheet_PlaceTable.Trim_CenterTable margin ->
                        Sheet_PlaceTable.Trim_CenterTable (margin.Rotate(rotation))
                    | Sheet_PlaceTable.At _ -> 
                        failwithf 
                            "Not supported: AlignDirection is Vertical and Sheet_PlaceTable is %A" args.Sheet_PlaceTable
                    
                CellRotation =
                    match rotation with 
                    | Rotation.None
                    | Rotation.R180 -> args.CellRotation
                    | Rotation.Clockwise
                    | Rotation.Counterclockwise ->
                        match args.CellRotation with
                        | CellRotation.None -> CellRotation.None
                        | CellRotation.R180WhenColNumIsEven -> CellRotation.R180WhenRowNumIsEven
                        | CellRotation.R180WhenRowNumIsEven -> CellRotation.R180WhenColNumIsEven

                Background = args.Background.Rotate(rotation)
            }

        member x.TrySetSpaceMiddleLineWidth(width) =
            { x with 
                HSpaceExes = x.HSpaceExes.TrySetMiddleLineProps(fun props ->
                    { props with LineWidth = width }
                )

                VSpaceExes = x.VSpaceExes.TrySetMiddleLineProps(fun props ->
                    { props with LineWidth = width }
                )
            }

        member x.TrySetMarkLineWidth(width) =
            { x with 
                Cropmark =
                    match x.Cropmark with 
                    | Some v ->
                        { v with Width = width }
                        |> Some
                    | None -> None
            }
                .TrySetSpaceMiddleLineWidth(width)

        static member DefaultValue =
            {
                ColNums = [0]
                RowNum = 0
                Cropmark = None
                HSpaceExes =  Spaces.Zero
                VSpaceExes = Spaces.Zero
                UseBleed = false
                Background = Background.Size FsSize.A4
                Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(0.))
                DesiredSizeOp = None
                IsRepeated = false
                CellRotation = CellRotation.None
                DesiredPageOrientation = DesiredPageOrientation.Automatic
            }

    type ImposingArguments = private ImposingArguments of fillingMode: FillingMode * value: _ImposingArguments
    with 
        member x.Value =
            let (ImposingArguments (fillingMode, value)) = x
            value

        member x.FillingMode = 
            let (ImposingArguments (fillingMode, value)) = x
            fillingMode

        static member Create(mapping: _ImposingArguments -> _ImposingArguments) = 
            let args = mapping _ImposingArguments.DefaultValue
            let args = 
                { 
                    args with 
                        ColNums = 
                            match args.ColNums with 
                            | [] -> [0]
                            | _ -> args.ColNums

                        HSpaceExes = 
                            match args.HSpaces with 
                            | [] -> Spaces.Zero
                            | _ -> args.HSpaceExes

                        VSpaceExes = 
                            match args.VSpaces with 
                            | [] -> Spaces.Zero
                            | _ -> args.VSpaceExes

                        DesiredPageOrientation =
                            match args.Background with 
                            | Background.Size _ -> args.DesiredPageOrientation
                            | Background.File backgroundFile ->
                                match BackgroundFile.getSize backgroundFile with 
                                | FsSize.Landscape _ -> DesiredPageOrientation.Landscape
                                | FsSize.Portrait _ -> DesiredPageOrientation.Portrait
                                | FsSize.Uniform -> DesiredPageOrientation.Landscape
                }


            let isValidSheet_PlaceTable = 
                match args.Sheet_PlaceTable with 
                | Sheet_PlaceTable.Trim_CenterTable margin -> 
                    [ margin.Left; margin.Top; margin.Right; margin.Bottom ]
                    |> List.forall(fun m -> m >= 0.)
                | Sheet_PlaceTable.At position ->
                    match position with
                    | Position.Left (x, _) -> x >= 0.
                    | Position.Right (x, _) -> x <= 0.
                    | Position.XCenter (x, y) -> true
                    && 
                        match position with
                        | Position.Top (_, y) -> y <= 0.
                        | Position.Bottom (_, y) -> y >= 0.
                        | Position.YCenter _ -> true

            if not isValidSheet_PlaceTable then failwithf "Invalid Sheet_PlaceTable %A" args.Sheet_PlaceTable

            let fillingMode =
                match List.distinct args.ColNums, args.RowNum with
                | [0], 0 ->
                    FillingMode.Automatic

                | uniqueValues, 0 when List.forall (fun m -> m > 0) uniqueValues ->
                    FillingMode.RowAutomatic { ColNums =  args.ColNums }

                | [0], rowNum when rowNum > 0 ->
                    FillingMode.ColumnAutomatic { RowNum =  args.RowNum }

                | uniqueValues, rowNum when List.forall (fun m -> m > 0) uniqueValues && rowNum > 0 ->
                    FillingMode.Numeric { ColNums = args.ColNums; RowNum = rowNum }

                | [], _ -> failwith "colNums cannot be empty"

                | _ -> failwith "Invalid token"

            ImposingArguments(fillingMode, args)


    type private ItemPosition =
        | First = 0 
        | Middle = 1
        | Last = 2
        | FirstAndLast = 3



    /// coordinate origin is left top of table
    /// y: top -> bottom --------0 -> tableHeight
    type ImposingCell = 
        { Page: PdfPage
          Size: FsSize
          X: float
          Y: float
          Index: int
          ImposingRow: ImposingRow }

    with 
        member private x.ImposingSheet: ImposingSheet = x.ImposingRow.ImposingSheet

        member private x.ImposingDocument: ImposingDocument = x.ImposingSheet.ImposingDocument

        member private x.SplitDocument = x.ImposingDocument.SplitDocument

        member internal x.Offset(offset: FsPoint) =
            { x with 
                X = x.X + offset.X
                Y = x.Y + offset.Y }

        member x.ImposingArguments: ImposingArguments = x.ImposingDocument.ImposingArguments
        
        member x.HSpaces = x.ImposingArguments.Value.HSpaces

        member x.HSpaceExes = x.ImposingArguments.Value.HSpaceExes

        member x.VSpaces = x.ImposingArguments.Value.VSpaces

        member x.VSpaceExes = x.ImposingArguments.Value.VSpaceExes

        member x.Cropmark = x.ImposingArguments.Value.Cropmark

        member x.UseBleed = x.ImposingArguments.Value.UseBleed

        member private cell.ColumnPosition() =
            let cells = cell.ImposingRow.Cells
            let index = cell.Index
            if cells.Count = 1 then ItemPosition.FirstAndLast
            else
                match index with 
                | 0 -> ItemPosition.First
                | index when index = cells.Count - 1 -> ItemPosition.Last
                | _ -> ItemPosition.Middle 

        member private cell.RowPosition() =
            let row = cell.ImposingRow
            let rows = row.ImposingSheet.Rows
            let rowIndex = row.RowIndex
            let index = rowIndex
            if rows.Count = 1 then ItemPosition.FirstAndLast
            else
                match  index with 
                | 0 -> ItemPosition.First
                | index when index = rows.Count - 1 -> ItemPosition.Last
                | _ -> ItemPosition.Middle


        member private cell.GetXObjectContentBox() =
            let xObjectContentBox = 
                if cell.UseBleed
                then PdfPage.getTrimBox cell.Page
                else cell.Page.GetActualBox()

            xObjectContentBox

        member private cell.GetScale() =
            let xObjectContentBox = cell.GetXObjectContentBox()
                
            let size = RoundedSize.Create cell.Size
            //let size = cell.Size
            let scaleX = size.Width / xObjectContentBox.GetWidthF() 
            let scaleY = size.Height / xObjectContentBox.GetHeightF() 
            scaleX, scaleY

        member private cell.GetClippedXObject() =
            let scaleX, scaleY = cell.GetScale()


            let xobject = cell.Page.CopyAsFormXObject(cell.SplitDocument.Writer)
            if cell.UseBleed
            then 
                let trimBox = PdfPage.getTrimBox cell.Page
                        
                let actualBox = cell.Page.GetActualBox()

                let cells = cell.ImposingRow.Cells

                if Rectangle.equal trimBox actualBox then xobject
                else 
                    let bbox = 
                        let x, width =
                            
                            let index = cell.Index

                            match cell.ColumnPosition() with 
                            | ItemPosition.First   ->
                                let x = actualBox.GetXF()
                                
                                let width = 
                                    let nextHSpace = cell.HSpaces.[(index) % cell.HSpaces.Length] / scaleX
                                    trimBox.GetRightF() - actualBox.GetLeftF() + nextHSpace / 2.

                                x, width

                            | ItemPosition.FirstAndLast ->  actualBox.GetXF() , actualBox.GetWidthF()

                            | ItemPosition.Middle ->
                                let preHSpace = cell.HSpaces.[(index - 1) % cell.HSpaces.Length] / scaleX
                                let nextHSpace = cell.HSpaces.[(index) % cell.HSpaces.Length] / scaleX

                                let x = trimBox.GetXF() - preHSpace / 2.
                                let width = trimBox.GetWidthF() + nextHSpace / 2. + preHSpace / 2.
                                x, width

             

                            | ItemPosition.Last ->
                                let preHSpace = cell.HSpaces.[(index - 1) % cell.HSpaces.Length] / scaleX

                                let x = trimBox.GetXF() - preHSpace / 2.

                                let width = actualBox.GetRightF() - trimBox.GetLeftF() + preHSpace / 2. 

                                x,width



                        let y, height = 
                            let row = cell.ImposingRow
                            let index = row.RowIndex

                            match cell.RowPosition() with 
                            | ItemPosition.First ->
                                let nextVSpace = cell.VSpaces.[(index) % cell.VSpaces.Length] / scaleY

                                let y = trimBox.GetYF() - nextVSpace / 2.

                                let height = actualBox.GetTopF() - trimBox.GetBottomF() + nextVSpace / 2.
                                y, height

                            | ItemPosition.Middle ->
                                let preVSpace = cell.VSpaces.[(index - 1) % cell.VSpaces.Length] / scaleY

                                let nextVSpace = cell.VSpaces.[(index) % cell.VSpaces.Length] / scaleY

                                let y = trimBox.GetYF() - nextVSpace / 2.
                                let height = trimBox.GetHeightF() + nextVSpace / 2. + preVSpace / 2.
                                y, height

                            | ItemPosition.FirstAndLast -> actualBox.GetYF(), actualBox.GetHeightF()
                            | ItemPosition.Last  -> 
                                let y = actualBox.GetYF()

                                let preVSpace = cell.VSpaces.[(index - 1) % cell.VSpaces.Length] / scaleY

                                let height = trimBox.GetTopF() - actualBox.GetBottomF() + preVSpace / 2.

                                y, height


                        Rectangle.create x y width height

                    let resetBBoxWhenTrimBoxIsNotInsideActualBox (bbox: Rectangle) =
                        let x = max (actualBox.GetXF()) (bbox.GetXF())
                        let width = min (actualBox.GetWidthF()) (bbox.GetWidthF())

                        let y = max (actualBox.GetYF()) (bbox.GetYF())
                        let height = min (actualBox.GetHeightF()) (bbox.GetHeightF())

                        Rectangle.create x y width height

                    let bbox = resetBBoxWhenTrimBoxIsNotInsideActualBox bbox

                    xobject.SetBBox(Rectangle.toPdfArray bbox) 

            else xobject

        static member AddToPdfCanvas (pageMargin: Margin) (cellContentAreas_ExtendByCropmarkDistance: Rectangle list) (pdfCanvas: PdfCanvas) (cell: ImposingCell) =
            let addXObject (pdfCanvas: PdfCanvas) = 
                let scaleX, scaleY = cell.GetScale()
                let xObject = cell.GetClippedXObject()
                let xObjectContentBox = cell.GetXObjectContentBox()

                //let affineTransform_Rotate =
                //    match cell.ImposingArguments.Value.CellRotation with 
                //    | CellRotation.None -> AffineTransform.GetTranslateInstance(0., 0.)
                //    | CellRotation.R180WhenColNumIsEven ->
                //        let index = cell.Index
                //        if (index + 1) % 2 = 0 
                //        then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., xObjectContentBox.GetXF() + xObjectContentBox.GetWidthF() / 2., xObjectContentBox.GetYF() + xObjectContentBox.GetHeightF() / 2.  )
                //        //then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., cell.Size.Width / 2., cell.Size.Height / 2.  )
                //        else AffineTransform.GetTranslateInstance(0., 0.)
                //    | CellRotation.R180WhenRowNumIsEven ->
                //        let index = cell.ImposingRow.RowIndex
                //        if (index + 1) % 2 = 0
                //        then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., xObjectContentBox.GetXF() + xObjectContentBox.GetWidthF() / 2., xObjectContentBox.GetYF() + xObjectContentBox.GetHeightF() / 2.  )
                //        //then AffineTransform.GetRotateInstance(- Math.PI / 180. * 180., xObjectContentBox.GetXF() + xObjectContentBox.GetWidthF() / 2., xObjectContentBox.GetYF() + xObjectContentBox.GetHeightF() / 2.  )
                //        else AffineTransform.GetTranslateInstance(0., 0.)

                let affineTransform_Scale =
                    AffineTransform.GetScaleInstance(scaleX, scaleY)

                let affineTransform_Translate0 =
                    AffineTransform.GetTranslateInstance(-xObjectContentBox.GetXF(), -xObjectContentBox.GetYF())

                let affineTransform_Translate1 =
                    AffineTransform.GetTranslateInstance(cell.X, -(cell.Size.Height + cell.Y) )


                let affineTransform = affineTransform_Translate0.Clone()
                //affineTransform.Concatenate(affineTransform_Rotate)
                affineTransform.PreConcatenate(affineTransform_Scale)
                affineTransform.PreConcatenate(affineTransform_Translate1)

                pdfCanvas.AddXObject(xObject, affineTransform)

            let addCropmarks (pdfCanvas: PdfCanvas) =
                match cell.Cropmark with 
                | Some cropmark ->
                    pdfCanvas
                        .SetLineWidth(float32 cropmark.Width) 
                        .SetStrokeColor(NullablePdfCanvasColor.OfPdfCanvasColor cropmark.Color)
                        |> ignore

                    let height = cell.Size.Height

                    let allCropmarkLines = 
                        let x = cell.X
                        let y = cell.Y
                        let distance = cropmark.Distance
                        let length = cropmark.Length
                        let width = cell.Size.Width
                        let colPos = cell.ColumnPosition()
                        let rowPos = cell.RowPosition()

                        let leftTopV = x, y-distance, x, y-distance-length                                   
                        let leftTopH = x-distance, y, x-distance-length, y            
                        let leftBottomV = x, y+distance+height, x, y+distance+length+height
                        let leftBottomH = x-distance, y+height, x-distance-length, y+height     
                        let rightTopV = x+width, y-distance, x+width, y-distance-length  
                        let rightTopH = x+distance+width, y, x+distance+length+width, y        
                        let rightBottomV = x+width, y+distance+height, x+width, y+distance+length+height
                        let rightBottomH = x+distance+width, y+height, x+distance+length+width, y+height
                        match cropmark.IsRevealedBetweenCells with 
                        | true ->
                            [
                                leftTopV
                                leftTopH
                                leftBottomV
                                leftBottomH
                                rightTopV
                                rightTopH
                                rightBottomV
                                rightBottomH
                            ] 
                        | false ->
                            [
                                match rowPos with 
                                | ItemPosition.First | ItemPosition.FirstAndLast -> 
                                    leftTopV
                                    rightTopV

                                | _ -> ()

                                match colPos with 
                                | ItemPosition.First | ItemPosition.FirstAndLast -> 
                                    leftTopH
                                    leftBottomH
                                | _ -> ()

                                match rowPos with 
                                | ItemPosition.FirstAndLast | ItemPosition.Last ->
                                    leftBottomV
                                    rightBottomV
                                | _ -> ()

                                match colPos with 
                                | ItemPosition.FirstAndLast | ItemPosition.Last ->
                                    rightTopH
                                    rightBottomH
                                | _ -> ()
                            ] 

                        |> List.map (fun (x1, y1, x2 , y2) ->
                            { Start = new Point(x1, y1); End = new Point(x2, y2) }
                        )
                    
                    let interspatialCropmarkLines = 
                        allCropmarkLines
                        |> List.filter (fun l -> List.forall (fun cropamrkRedArea -> l.IsOutsideOf(cropamrkRedArea)) cellContentAreas_ExtendByCropmarkDistance)
                        

                    (pdfCanvas, interspatialCropmarkLines)
                    ||> List.fold (fun (pdfCanvas: PdfCanvas) line ->
                        pdfCanvas
                            .MoveTo(line.Start.x, -(line.Start.y))
                            .LineTo(line.End.x, -(line.End.y))
                            .Stroke()
                    )

                | None -> pdfCanvas

            let addSpaceMiddleLine (pdfCanvas: PdfCanvas) =  
                let row_cells = cell.ImposingRow.Cells

                let colIndex = row_cells.IndexOf(cell)
                let rowIndex = cell.ImposingSheet.Rows.IndexOf(cell.ImposingRow)

                let preHSpace =     
                    match colIndex with 
                    | 0 -> 0.
                    | _ -> cell.HSpaces.[(colIndex - 1) % cell.HSpaces.Length] 
                let nextHSpaceEx = cell.HSpaceExes.Value.[(colIndex) % cell.HSpaces.Length] 
                let nextHSpace = nextHSpaceEx.Space 

                let preVSpace = 
                    match rowIndex with 
                    | 0 -> 0. 
                    | _ -> cell.VSpaces.[(rowIndex - 1) % cell.VSpaces.Length] 

                let nextVSpaceEx = cell.VSpaceExes.Value.[(rowIndex) % cell.VSpaces.Length] 
                let nextVSpace = nextVSpaceEx.Space


                let addVertical() =
                    match nextHSpaceEx.MiddleLine with 
                    | Some spaceMiddleLine -> 
                        let pdfCanvasAddLineArguments = spaceMiddleLine.Properties


                        let nextCell = 
                            row_cells
                            |> Seq.tryItem (colIndex+1)

                        match nextCell with 
                        | Some nextCell ->
                            let line: StraightLine =
                                let x = cell.X + nextHSpace / 2. + cell.Size.Width  
                                let yStart = 
                                    match rowIndex with 
                                    | 0 -> 
                                        let margin = pageMargin
                                        margin.Top * spaceMiddleLine.EdgeLength_PercentToMargin + -cell.Y 

                                    | _ -> -cell.Y + preVSpace / 2.
                                let yEnd = 
                                    match rowIndex = cell.ImposingSheet.RowsCount-1 with 
                                    | true ->
                                        let margin = pageMargin
                                        -cell.Y - cell.Size.Height - margin.Bottom * spaceMiddleLine.EdgeLength_PercentToMargin
                                    | false ->
                                        -cell.Y - cell.Size.Height - nextVSpace / 2.

                                { Start = Point(x, yStart); End = Point(x, yEnd) }
                            
                            PdfCanvas.addLine line (fun args -> pdfCanvasAddLineArguments ) pdfCanvas

                        | None -> pdfCanvas
                    | None -> pdfCanvas

                let addHorizontal() =
                    match nextVSpaceEx.MiddleLine with 
                    | Some spaceMiddleLine -> 
                        let pdfCanvasAddLineArguments = spaceMiddleLine.Properties
                        let nextRow = 
                            cell.ImposingSheet.Rows
                            |> Seq.tryItem (rowIndex+1)

                        match nextRow with 
                        | Some nextRow ->
                            let line: StraightLine =
                                let y = -cell.Y - cell.Size.Height - nextVSpace / 2.
                                let xStart = 
                                    match colIndex with 
                                    | 0 -> 
                                        let margin = pageMargin
                                        cell.X - margin.Left * spaceMiddleLine.EdgeLength_PercentToMargin

                                    | _ -> cell.X - preHSpace / 2.

                                let xEnd = 
                                    match colIndex = cell.ImposingRow.Cells.Count-1 with 
                                    | true ->
                                        let margin = pageMargin
                                        cell.X + cell.Size.Width + margin.Right * spaceMiddleLine.EdgeLength_PercentToMargin
                                    | false -> cell.X + cell.Size.Width + nextHSpace / 2.

                                { Start = Point(xStart, y); End = Point(xEnd, y) }
                            
                            PdfCanvas.addLine line (fun args -> pdfCanvasAddLineArguments ) pdfCanvas

                        | None -> pdfCanvas
                    | None -> pdfCanvas


                pdfCanvas.SaveState() |> ignore
                addVertical() |> ignore
                addHorizontal() |> ignore
                pdfCanvas.RestoreState()


            pdfCanvas
            |> addXObject
            |> addCropmarks
            |> addSpaceMiddleLine

    and ImposingRow(sheet: ImposingSheet, rowIndex: int) = 
        let mutable cells = ResizeArray<ImposingCell>()
        let mutable x = 0.

        member private x.ImposingArguments : ImposingArguments  = sheet.ImposingDocument.ImposingArguments

        member internal x.ImposingSheet = sheet

        member internal x.RowIndex = rowIndex


            

        member internal x.AddToCanvas pageMargin cellContentAreas_ExtendByCropmarkDistance (pdfCanvas: PdfCanvas) = 
            (pdfCanvas, cells)
            ||> Seq.fold (ImposingCell.AddToPdfCanvas pageMargin cellContentAreas_ExtendByCropmarkDistance)


        member internal this.Push(readerPage: PdfPage) =
            let pageSize = sheet.PageSize
            let args = this.ImposingArguments.Value
            let fillingMode = this.ImposingArguments.FillingMode

            let newCell =
                { Page = readerPage 
                  Size = match args.DesiredSizeOp with 
                          | Some size -> size 
                          | None -> 
                                if args.UseBleed then FsSize.ofRectangle(readerPage.GetTrimBox())
                                else FsSize.ofRectangle(readerPage.GetActualBox()) 
                          
                  X = x
                  Y = sheet.Y
                  Index = cells.Count
                  ImposingRow = this }

            if newCell.Size.Width > pageSize.Width + tolerance.Value || newCell.Size.Height > pageSize.Height + tolerance.Value
            then failwithf "desired size %A is exceeded to sheet page size %A" newCell.Size pageSize
                  
            let addNewCell_UpdateState() =
                x <- x + newCell.Size.Width + args.HSpaces.[cells.Count % args.HSpaces.Length]
                cells.Add(newCell)
                true


            let willWidthExceedPageWidth = 
                let pageSize = pageSize
                let marginX =
                    match args.Sheet_PlaceTable with 
                    | Sheet_PlaceTable.Trim_CenterTable margin ->
                        margin.Left + margin.Right
                    | Sheet_PlaceTable.At position ->
                        match position with 
                        | Position.Left (x, _) -> x
                        | Position.Right (x, _) -> -x
                        | Position.XCenter(x, _) -> abs x

                newCell.X + newCell.Size.Width + marginX > pageSize.Width + tolerance.Value
                

            if willWidthExceedPageWidth then false
            else 
                match fillingMode with 
                | FillingMode.ColNumsSpecific colNums ->
                    if cells.Count = colNums.[(sheet.RowsCount - 1) % colNums.Length] then
                        false
                    else
                        addNewCell_UpdateState()

                | _ -> addNewCell_UpdateState()

        member internal this.Cells: ResizeArray<ImposingCell> = cells

        member this.Height = 
            if cells.Count = 0 then 0.
            else
                cells
                |> Seq.map (fun cell -> cell.Size.Height)
                |> Seq.max

        member this.Width =
            match Seq.tryLast cells with 
            | Some cell -> cell.X + cell.Size.Width
            | None -> 0.

        member this.Y =
            match Seq.tryHead cells with 
            | Some cell -> cell.Y
            | None -> 0.

        member internal x.Offset(offset: FsPoint) =
            let newCells = new ResizeArray<_>()
            for cell in cells do 
                cell.Offset(offset)
                |> newCells.Add

            cells <- newCells

        member x.GetCells() = List.ofSeq cells


    and ImposingSheet(imposingDocument: ImposingDocument, pageOrientation: PageOrientation, pageSize: FsSize) =
        let pageSize = 
            let size = pageSize
            FsPageSize.create (size) pageOrientation

        let rows = ResizeArray<ImposingRow>()

        let mutable y = 0.

        member private x.SplitDocument : SplitDocument = imposingDocument.SplitDocument

        member x.ImposingArguments : ImposingArguments  = imposingDocument.ImposingArguments

        member x.PageSize: FsPageSize = pageSize

        member internal x.Offset(offsets: FsPoint list) =
            rows
            |> Seq.iteri(fun i row ->
                row.Offset(offsets.[i])
            )
                

        member x.ImposingDocument: ImposingDocument = imposingDocument

        member internal x.Y = y

        member internal x.Push(readerPage: PdfPage) =

            let args = x.ImposingArguments.Value
            let fillingMode = x.ImposingArguments.FillingMode

            let addNewRow_UpdateState_PushAgain() =
                match Seq.tryLast rows with 
                | None -> ()
                | Some lastRow -> 
                    if lastRow.Cells.Count = 0 then
                        failwithf "Cannot push page to imposing sheet,please check your imposing arguments %A" x.ImposingArguments
                    y <- lastRow.Height + y + args.VSpaces.[(rows.Count - 1) % args.VSpaces.Length]

                let newRow = new ImposingRow(x, rows.Count)
                rows.Add(newRow)
                x.Push(readerPage)


            if rows.Count = 0 
            then addNewRow_UpdateState_PushAgain()
            else 
                let row = rows.Last()
                if row.Push(readerPage)
                then 
                    let removeLastRow_UpdateState() =
                        rows.RemoveAt(rows.Count - 1)
                        |> ignore
                                
                        let newLastRow = rows.[rows.Count - 1]
                        y <- newLastRow.Cells.[0].Y

                    let heightExceedPageHeight =
                        match Seq.tryLast rows with 
                        | Some row -> 
                            let marginY =
                                match args.Sheet_PlaceTable with 
                                | Sheet_PlaceTable.Trim_CenterTable margin ->
                                    margin.Top + margin.Bottom
                                | Sheet_PlaceTable.At position ->
                                    match position with 
                                    | Position.Top (_, y) -> -y
                                    | Position.Bottom (_, y) -> y
                                    | Position.YCenter(_, y) -> abs y

                            row.Height + y + marginY > pageSize.Height + tolerance.Value

                        | None -> false

                    let rowNumExceedSpecificRowNumResult = 
                        match fillingMode with 
                        | FillingMode.RowNumSpecific rowNum -> rows.Count > rowNum 
                        | _ -> false

                    if heightExceedPageHeight 
                    then
                        let lastRow = rows.Last()
                        let cells = lastRow.Cells

                        if cells.Count = 1 
                        then
                            cells.RemoveAt(cells.Count - 1)
                        removeLastRow_UpdateState()

                        false
                    elif rowNumExceedSpecificRowNumResult 
                    then
                        removeLastRow_UpdateState()
                        false
                    else true
                else addNewRow_UpdateState_PushAgain()


        member this.Margin =
            let args = this.ImposingArguments.Value
            match args.Sheet_PlaceTable with 
            | Sheet_PlaceTable.Trim_CenterTable margin -> margin
            | Sheet_PlaceTable.At position ->
                let left = 
                    match position with 
                    | Position.Left (x, _) -> x
                    | Position.Right (x, _) -> this.Width - this.TableWidth + x
                    | Position.XCenter(x, _) -> (this.Width - this.TableWidth) / 2. + x

                let right = this.Width - this.TableWidth - left

                let bottom = 
                    match position with 
                    | Position.Top (_, y) -> this.Height - this.TableHeight + y
                    | Position.Bottom (_, y) -> y
                    | Position.YCenter(_, y) -> (this.Height - this.TableHeight) / 2. + y

                let top = this.Height - this.TableHeight - bottom

                Margin.Create(left, top, right, bottom)


        member internal this.Draw() =
            let args = this.ImposingArguments.Value

            let newPage = this.SplitDocument.Writer.AddNewPage()
            
            let pdfCanvas = PdfCanvas(newPage)

            let height = this.Height
            let width = this.Width

            let pageBox = 
                match args.Sheet_PlaceTable with 
                | Sheet_PlaceTable.Trim_CenterTable margin ->
                    (Rectangle.create -margin.Left -(height - margin.Top) width height)

                | Sheet_PlaceTable.At position ->
                    let x =
                        match position with
                        | Position.Left (x, _) -> x
                        | Position.Right (x, _) -> x + (width - this.TableWidth)
                        | Position.XCenter (x, _) -> (width - this.TableWidth) / 2. + x
                 
                    let y =
                        match position with 
                        | Position.Top (_, y) -> -y
                        | Position.Bottom (_, y) -> -y + height - this.TableHeight
                        | Position.YCenter(_, y) -> -y + (height - this.TableHeight) / 2.


                    (Rectangle.create -x (-height + y) width height) 

            PdfPage.setPageBox (PageBoxKind.AllBox) pageBox newPage |> ignore

            match args.Background with 
            | Background.File backgroudFile ->
                let reader = new PdfDocument(new PdfReader(backgroudFile.ClearedPdfFile.Path))
                let xobject = reader.GetPage(1).CopyAsFormXObject(this.SplitDocument.Writer)
                let bkRect = BackgroundFile.getPagebox backgroudFile
                let affineTransform = 
                    let angle = Rotation.getAngle backgroudFile.CurrentRotation
                    let x, y = pageBox.GetXF() - bkRect.GetXF() , pageBox.GetYF() - bkRect.GetYF()

                    let affineTransform_Rotate = AffineTransform.GetRotateInstance(Math.PI / -180. * angle, bkRect.GetXCenterF(), bkRect.GetYCenterF())
                
                    let affineTransform_Translate = 
                        { ScaleX = 1. 
                          ScaleY = 1. 
                          TranslateX = x
                          TranslateY = y
                          ShearX = 0.
                          ShearY = 0. }
                        |> AffineTransformRecord.toAffineTransform

                    affineTransform_Rotate.PreConcatenate(affineTransform_Translate)
                    affineTransform_Rotate

                pdfCanvas.AddXObject(xobject, affineTransform) |> ignore
                reader.Close()
            | _ -> ()

            let cellContentAreas_ExtendByCropmarkDistance = 
                match args.Cropmark with 
                | Some cropmark ->
                    let cells =
                        rows
                        |> Seq.collect (fun row -> row.Cells)
                    cells
                    |> List.ofSeq
                    |> List.map (fun cell ->
                        let rect = Rectangle.create cell.X cell.Y cell.Size.Width cell.Size.Height
                        Rectangle.applyMargin (Margin.Create (cropmark.Distance - tolerance.Value)) rect
                    )
                | _ -> []


            (pdfCanvas, rows)
            ||> Seq.fold (fun pdfCanvas row -> 
                row.AddToCanvas this.Margin cellContentAreas_ExtendByCropmarkDistance pdfCanvas
            )
            |> ignore




        member x.TableWidth = 
            let args = x.ImposingArguments.Value
            if rows.Count = 0 then 0.
            else 
                rows
                |> Seq.map (fun row -> row.Width)
                |> Seq.max


        member x.Width = 
            let args = x.ImposingArguments.Value
            match args.Sheet_PlaceTable with 
            | Sheet_PlaceTable.Trim_CenterTable margin ->
                margin.Left + margin.Right + x.TableWidth

            | Sheet_PlaceTable.At _ ->
                pageSize.Width

        member x.TableHeight =
            let args = x.ImposingArguments.Value
            match Seq.tryLast rows with 
            | Some row -> row.Height + x.Y
            | None -> 0.


        member x.Height = 
            let args = x.ImposingArguments.Value
            match args.Sheet_PlaceTable with 
            | Sheet_PlaceTable.Trim_CenterTable margin ->
                x.TableHeight + margin.Bottom + margin.Top

            | Sheet_PlaceTable.At _ ->
                pageSize.Height

            

        member internal x.Rows: ResizeArray<ImposingRow> = rows
        
        member x.GetRows() = List.ofSeq rows

        member x.GetFirstRow() = rows.[0]

        member x.RowsCount = rows.Count 

        member x.GetCellsCount() = rows |> Seq.sumBy(fun row -> row.Cells.Count)

        member private x.CellsCount = x.GetCellsCount()

    /// Build() -> Draw()
    and ImposingDocument (splitDocument: SplitDocument, imposingArguments: ImposingArguments) =  
        let sheets = new ResizeArray<ImposingSheet>()
        let mutable isDrawed = false


        let mutable imposingArguments = imposingArguments

        member internal x.SplitDocument: SplitDocument = splitDocument

        member x.ImposingArguments: ImposingArguments = imposingArguments

        /// CRACKING FOR IMPOSE WITH CellRotation
        member internal x.ReSetIsRepeated(isRepeated) =
            imposingArguments <- 
                ImposingArguments.Create(fun _ -> { imposingArguments.Value with IsRepeated = isRepeated})

        member internal x.IsDrawed = isDrawed

        member internal x.Offset(offsetLists: FsPoint list list) =
            sheets
            |> Seq.iteri(fun i sheet ->
                sheet.Offset(offsetLists.[i])
                
            )

        /// NOTE: Internal Use
        member internal x.Draw() =
            if sheets.Count = 0 then failwith "cannot draw documents, sheets is empty, please invoke Build() first"
            match isDrawed with 
            | true -> failwith "document was drawed already"
            | false ->
                for sheet in sheets do
                    sheet.Draw()
                    isDrawed <- true

        /// NOTE: Internal Use
        member internal x.Build() =
            let args = x.ImposingArguments.Value
            sheets.Clear()

            let reader = splitDocument.Reader

            let readerPages = PdfDocument.getPages reader

            let rec produceSheet pageSize readerPages (sheet: ImposingSheet) =
                match readerPages with 
                | (readerPage : PdfPage) :: t ->
                    
                    if sheet.Push(readerPage) 
                    then 
                        if args.IsRepeated 
                        then produceSheet pageSize readerPages sheet
                        else produceSheet pageSize t sheet
                    else 

                        match args.IsRepeated, t.Length with 
                        | true, i when i > 0 ->
                            sheet, t

                        | true, 0 -> sheet, []

                        | false, _ -> 
                            sheet, readerPages

                        | _ -> failwith "Invalid token"
                | [] -> sheet, []


            let rec produceSheets pageSize desiredPageOriention (readerPages: PdfPage list) (sheets: ImposingSheet list) =
                if readerPages.Length = 0 then sheets
                else
                    match desiredPageOriention with 
                    | DesiredPageOrientation.PageOrientation pageOrientation ->
                        let producedSheet, leftPages = 
                            produceSheet pageSize readerPages (new ImposingSheet(x, pageOrientation, pageSize))
                        produceSheets pageSize desiredPageOriention leftPages (producedSheet :: sheets)

                    | DesiredPageOrientation.Automatic ->

                        let producedSheet, leftPages = 
                            let producedSheet1, leftPages1 = 
                                produceSheet pageSize readerPages (new ImposingSheet(x, PageOrientation.Landscape, pageSize))

                            let producedSheet2, leftPages2 = 
                                produceSheet pageSize readerPages (new ImposingSheet(x, PageOrientation.Portrait, pageSize))
                        
                            if 
                                (args.IsRepeated && producedSheet1.GetCellsCount() > producedSheet2.GetCellsCount())
                                || (not args.IsRepeated && leftPages1.Length < leftPages2.Length)
                            then producedSheet1, leftPages1
                            else producedSheet2, leftPages2

                        produceSheets pageSize desiredPageOriention leftPages (producedSheet :: sheets)

                    | _ -> failwith "Invalid token"


            let producedSheets = 
                let cellSizes = 
                    readerPages
                    |> List.map (fun readerPage ->
                        match args.DesiredSizeOp with 
                        | Some size -> size 
                        | None -> 
                              if args.UseBleed then FsSize.ofRectangle(readerPage.GetTrimBox())
                              else FsSize.ofRectangle(readerPage.GetActualBox()) 
                    )

                let desiredPageOriention, pageSize =
                    let margin =
                        match args.Sheet_PlaceTable with 
                        | Sheet_PlaceTable.Trim_CenterTable margin -> margin
                        | Sheet_PlaceTable.At _ -> Margin.Zero

                    let backgroundSize =
                         Background.getSize args.Background

                    let (|ValidOrientation|InvalidOrientation|) (orientation, backgroundSize) =
                        let backgroundSize = 
                            match orientation with 
                            | PageOrientation.Landscape -> FsSize.landscape backgroundSize
                            | PageOrientation.Portrait -> FsSize.portrait backgroundSize

                    
                        let exceedHorizontal =
                            cellSizes 
                            |> List.tryFind(fun cellSize ->
                                cellSize.Width + margin.Left + margin.Right >= backgroundSize.Width + tolerance.Value
                            )

                        let exceedVertical =
                            cellSizes 
                            |> List.tryFind(fun cellSize ->
                                cellSize.Height + margin.Top + margin.Bottom >= backgroundSize.Height + tolerance.Value
                            )

                        match exceedHorizontal, exceedVertical with 
                        | None _, None _ -> ValidOrientation
                        | Some v, _ 
                        | _, Some v -> InvalidOrientation v


                    match args.DesiredPageOrientation with 
                    | DesiredPageOrientation.Automatic  ->
                        let getDesiredPageOrientation (backgroundSize) =
                            match (PageOrientation.Portrait, backgroundSize) , (PageOrientation.Landscape, backgroundSize) with 
                            | ValidOrientation, ValidOrientation ->     Result.Ok DesiredPageOrientation.Automatic
                            | ValidOrientation, InvalidOrientation _ -> Result.Ok DesiredPageOrientation.Portrait
                            | InvalidOrientation _, ValidOrientation -> Result.Ok DesiredPageOrientation.Landscape
                            | InvalidOrientation size, InvalidOrientation _ -> Result.Error size

                        match getDesiredPageOrientation backgroundSize with 
                        | Result.Ok desiredPageOrientation -> desiredPageOrientation, backgroundSize
                        | Result.Error size ->

                            match args.Background with 
                            | Background.Size backgroundSize ->
                                let backgroundSize_extented_byMargin = backgroundSize |> FsSize.applyMargin margin
                                match getDesiredPageOrientation backgroundSize_extented_byMargin with 
                                | Result.Ok desiredPageOrientation -> 
                                    desiredPageOrientation, backgroundSize_extented_byMargin
                                | Result.Error size -> 
                                    failwithf "desired size %A + margin %A is exceeded to sheet page size %A" size margin backgroundSize


                            | Background.File _ ->
                                failwithf "desired size %A + margin %A is exceeded to sheet page size %A" size margin backgroundSize

                    | _ -> 
                        let pageOrientation =
                            match args.DesiredPageOrientation with 
                            | DesiredPageOrientation.Portrait -> PageOrientation.Portrait
                            | DesiredPageOrientation.Landscape -> PageOrientation.Landscape
                            | _ -> failwith "Invalid token"
                                

                        match (pageOrientation, backgroundSize) with 
                        | ValidOrientation -> args.DesiredPageOrientation, backgroundSize
                        | InvalidOrientation _ ->
                            let backgroundSize_extented_byMargin = backgroundSize |> FsSize.applyMargin margin
                            match (pageOrientation, backgroundSize_extented_byMargin) with 
                            | ValidOrientation -> args.DesiredPageOrientation, backgroundSize_extented_byMargin
                            | InvalidOrientation size ->
                                failwithf "desired size %A + margin %A is exceeded to sheet page size %A" size margin backgroundSize
                                


                produceSheets pageSize desiredPageOriention readerPages []

            sheets.AddRange(List.rev producedSheets)

        member x.GetFirstCell() = sheets.[0].Rows.[0].Cells.[0]

        member x.GetFirstSheet() = sheets.[0]

        member x.GetFirstRow() = sheets.[0].Rows.[0]

        member x.GetSheets() = List.ofSeq sheets

        member x.GetSheet(index) = sheets.[index]


    [<RequireQualifiedAccess>]
    module private Margin =
        let scaleX (scaleX: float) (margin: Margin) =
            if margin.Left < 0. || margin.Right < 0. then
                failwith "Not implemented"

            { margin with
                  Left = margin.Left * scaleX
                  Right = margin.Right * scaleX }

        let scaleY (scaleY: float) (margin: Margin) =
            if margin.Top < 0. || margin.Bottom < 0. then
                failwith "Not implemented"

            { margin with
                  Left = margin.Right * scaleY
                  Right = margin.Right * scaleY }



    type RotatableImposingSheet =
        { TableSize: FsSize
          CellsCount: int
          OriginSheet: ImposingSheet
          CurrentRotation: Rotation
          Margin: Margin }


        static member Create(imposingSheet: ImposingSheet) =
            { TableSize = {Width = imposingSheet.TableWidth; Height = imposingSheet.TableHeight }
              CellsCount = imposingSheet.GetCellsCount()
              OriginSheet = imposingSheet
              CurrentRotation = Rotation.None
              Margin = imposingSheet.Margin }

        member x.TableWidth = x.TableSize.Width

        member x.RotatedImposingArguments = x.OriginSheet.ImposingArguments.Value.Rotate(x.CurrentRotation)

        member x.OriginImposingArguments = x.OriginSheet.ImposingArguments.Value

        member x.TableHeight = x.TableSize.Height

        member x.SheetWidth =
            x.TableWidth + x.Margin.Left + x.Margin.Right

        member x.SheetHeight =
            x.TableHeight + x.Margin.Bottom + x.Margin.Top

        member x.SheetSize : FsSize =
            { Width = x.SheetWidth
              Height = x.SheetHeight }

        member x.ScaleToTargetSize(targetSize: FsSize) =
            let scaleX = targetSize.Width / x.SheetWidth
            let scaleY = targetSize.Height / x.SheetHeight

            { x with
                  TableSize = 
                    { Width = x.TableWidth * scaleX
                      Height = x.TableHeight * scaleY }

                  Margin =
                      x.Margin
                      |> Margin.scaleX scaleX
                      |> Margin.scaleY scaleY }

        member x.Rotate(rotation: Rotation) =
            { x with
                  TableSize = x.TableSize |> FsSize.rotate rotation
                  Margin = x.Margin.Rotate(rotation)
                  CurrentRotation = Rotation.concatenate rotation x.CurrentRotation
            }


    type RotatableImposingSheets =
        | RotatableImposingSheets of RotatableImposingSheet list
        member x.Value =
            let (RotatableImposingSheets value) = x
            value

        member x.Rotate(rotation) =
            x.Value
            |> List.map(fun m -> m.Rotate(rotation))
            |> RotatableImposingSheets

    type ImposingDocument with 
        member x.GetRotatableImposingSheets() =
            x.GetSheets()
            |> List.map RotatableImposingSheet.Create
            |> RotatableImposingSheets



    [<StructuredFormatDisplay("CellBound {Rect}")>]
    type CellBound = CellBound of FsRectangle
    with 
        member x.Rect =
            let (CellBound v) = x
            v

    type ImposingCell with 
        member x.GetBound(leftBottomCoordinate: FsPoint) =
            { 
                X = leftBottomCoordinate.X + x.X
                Y = leftBottomCoordinate.Y 
                Width = x.Size.Width
                Height = x.Size.Height
            }
            |> CellBound

    [<StructuredFormatDisplay("RowBound {Rect}")>]
    type RowBound = RowBound of CellBound list * spaces: float list
    with 
        member x.CellBounds =
            let (RowBound (cells, spaces)) = x
            cells



        member x.Height =
            let (RowBound (cells, spaces)) = x
            cells
            |> List.map(fun m -> m.Rect.Height)
            |> List.max

        member x.Width = 
            let (RowBound (cells, spaces)) = x
            let spaces = 
                List.replicate cells.Length spaces
                |> List.concat

            let totalCellsWidth =
                cells
                |> List.sumBy(fun m -> m.Rect.Width)

            let totalSpaces = 
                spaces.[0..cells.Length-2]
                |> List.sum


            totalCellsWidth + totalSpaces

        member x.X = 
            let (RowBound (cells, spaces)) = x
            cells.[0].Rect.X

        member x.Y = 
            let (RowBound (cells, spaces)) = x
            cells
            |> List.map(fun m -> m.Rect.Y)
            |> List.min

        member x.Rect =
            {
                X = x.X
                Y = x.Y
                Width = x.Width
                Height = x.Height
            }

        member x.LeftArea(width: float) =
            let rect = x.Rect
            let x = rect.X - width
            let y = rect.Y
            Rectangle.create x y width rect.Height

        member x.RightArea(width: float) =
            let rect = x.Rect
            let x = rect.Right
            let y = rect.Y
            Rectangle.create x y width rect.Height


    type ImposingRow with 
        member x.GetBound(leftBottomCoordinate: FsPoint) =
            let rowHeight = x.Height
            let cells = 
                x.Cells
                |> List.ofSeq
                |> List.map(fun m -> 
                    let leftBottomCoordinate =
                        { leftBottomCoordinate with 
                            Y = leftBottomCoordinate.Y + (rowHeight - m.Size.Height)
                        }
                    m.GetBound(leftBottomCoordinate)
                )

            let hspaces = x.ImposingArguments.Value.HSpaces
            RowBound(cells, hspaces)

        member private x.ZeroCoordinate_RowBound = x.GetBound(FsPoint.Zero)

    [<StructuredFormatDisplay("TableBound {Rect}")>]
    type TableBound = TableBound of RowBound list * spaces: float list
    with 
        member x.RowBounds =
            let (TableBound (rows, spaces)) = x
            rows


        member x.Rect =
            let (TableBound (rows, spaces)) = x
            let rowRects = 
                rows
                |> List.map(fun m -> m.Rect)

            let lastRowRect =
                rowRects
                |> List.last 

            { X = 
                rows
                |> List.map(fun m -> m.X)
                |> List.min

              Y = lastRowRect.Y
              Width =
                rowRects
                |> List.map(fun m -> m.Width)
                |> List.max

              Height =
                let totalSpaces = 
                    spaces.[0..rowRects.Length-2]
                    |> List.sum
                
                let totalHeight =
                    rowRects
                    |> List.sumBy(fun m -> m.Height)
                totalSpaces + totalHeight
            }

 

    type ImposingSheet with 
        member x.GetTableBound(leftBottomCoordinate: FsPoint) =
            let tableHeight = x.TableHeight
            let rows = 
                let rows = x.GetRows()
                rows
                |> List.mapi(fun i m -> 
                    let leftBottomCoordinate =
                        { leftBottomCoordinate with 
                            Y = leftBottomCoordinate.Y + tableHeight - m.Y - m.Height 
                        }
                    m.GetBound(leftBottomCoordinate)
                )

            TableBound(rows, x.ImposingArguments.Value.VSpaces)

        member x.GetTableBound(leftBottomCoordinate: Point) =
            x.GetTableBound(FsPoint.OfPoint leftBottomCoordinate)

        member private x.ZeroCoordinate_TableBound = x.GetTableBound(FsPoint.Zero)

    //type PdfPage with 
    //    member x.GetTableBound(sheet: ImposingSheet, tablePageKind: PageBoxKind) =
    //        let tablePageBox = x.GetPageBox(tablePageKind)