namespace Atrous.Pdf

module Types = 
    open iText.Kernel.Pdf
    open System
    open Fake.IO
    open iText.Kernel.Pdf.Canvas.Parser
    open iText.Kernel.Geom
    open iText.Kernel.Pdf.Xobject
    open iText.Kernel.Pdf.Canvas
    open System.IO
    open iText.Layout
    open System.Collections.Generic
    open iText.Kernel.Font
    open iText.Layout.Element
    open Atrous.Pdf.Colors
    open Atrous.Utils
    open Atrous.Pdf.Extensions
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open System.Collections.Concurrent
    open iText.Kernel.Colors

    type AnalyticSubpath =
        {
            Subpath: Subpath
            PathRenderInfo: PathRenderInfo
        }


    [<RequireQualifiedAccess>]
    module AnalyticSubpath =
        let getBound analyticSubpath =
            let ctm = analyticSubpath.PathRenderInfo.GetCtm()
            Subpath.getBound ctm analyticSubpath.Subpath
            
        let hasPoints analyticSubpath =
            Subpath.hasPoints analyticSubpath.Subpath

    type FontGen =
        {
            Name: string
            Gen: string -> PdfFont
        }

    [<RequireQualifiedAccess>]
    module FontGen =
        open iText.IO.Font
        let standard fontName =
            {
                Name = fontName
                Gen = fun _ -> PdfFontFactory.CreateFont fontName
            }

        let Helvetica = standard Constants.StandardFonts.HELVETICA
        let HelveticaBold = standard Constants.StandardFonts.HELVETICA_BOLD

    type TextArgument =
        {
            FontSize: float
            FontColor: Color
            FontGen: FontGen
            Text: string
        }

    type PdfDocumentWithResources =
        inherit PdfDocument
        val private fonts : ConcurrentDictionary<string,PdfFont>
        new (reader,writer) = { inherit PdfDocument(reader,writer);fonts = ConcurrentDictionary<string,PdfFont>() }
        new (writer: PdfWriter) =  { inherit PdfDocument(writer);fonts = ConcurrentDictionary<string,PdfFont>()}
        member this.Fonts = this.fonts

    [<RequireQualifiedAccess>]
    module PdfDocumentWithResources =
        let create (src: string) (dest: string) = 
            new PdfDocumentWithResources(new PdfReader(src),new PdfWriter(dest))
        let getOrAddFont fontGen (doc: PdfDocumentWithResources) =
            doc.Fonts.GetOrAdd(fontGen.Name,fun s ->
                fontGen.Gen s
            )
        let useFont fontGen (doc: PdfDocumentWithResources) f =
            let font = doc.Fonts.GetOrAdd(fontGen.Name,fun s ->
                fontGen.Gen s
            )
            f font

    let tmp src = src |> Path.changeExtension ".pdftmp" 
            
    [<RequireQualifiedAccess>]
    type PageBoxKind =
        | TrimBox
        | AllBox
        | CropBox

    type Margin = 
         {
             Left: float
             Top: float
             Right: float
             Bottom: float
         }

    [<RequireQualifiedAccess>]
    module Margin = 
            
        let createSimple l =
            {
                Left = l
                Top = l
                Right = l
                Bottom = l
            }

        let createWith f =
            createSimple 0. |> f

        let max m1 m2 =
            {
                Left = max m1.Left m2.Left
                Top = max m1.Top m2.Top
                Right = max m1.Right m2.Right
                Bottom = max m1.Bottom m2.Bottom
            }

        let toArray m =
            [|m.Top;m.Bottom;m.Left;m.Right|]
        
        let toArrayFromOption m =
            Option.map toArray m |> Option.defaultValue (Array.create 4 0.)
            
        let empty = 
            createSimple 0.
        
        let isEmpty m =
            m = empty

        let isNotEmpty m =
            m <> empty

    [<RequireQualifiedAccess>]
    module Rectangle = 

        let applyMargin (margin:Margin) (rect: Rectangle) =
            let left = margin.Left
            let top = margin.Top
            let right = margin.Right
            let bottom = margin.Bottom
            let x = rect.GetXF() - left
            let y = rect.GetYF() - bottom
            let width = rect.GetWidthF() + left + right 
            let height = rect.GetHeightF() + top + bottom
            Rectangle.create x y width height


    type FsSize = 
        {
            Width: float
            Height: float
        }



    [<RequireQualifiedAccess>]
    module FsSize =

        let create width height =
            {
                Width = width
                Height = height
            }


        let maximalBackground =
            create (toInche 5080) (toInche 5080)

        let ofRect (rect: Rectangle) = 
            {
                Height = rect.GetHeightF()
                Width = rect.GetWidthF()
            }

        let normallize (s: FsSize) =
            {
                Width = max s.Width s.Height
                Height = min s.Width s.Height
            }

        let isRotatedEqual size1 size2 =
            size1.Width @= size2.Height 
            && size1.Height @= size2.Width

        let rotate degree size = 
            if degree % 180. = 0. then
                size
            elif degree % 90. = 0. then
                {
                    Height = size.Width
                    Width = size.Height
                }
            else 
                failwith "Only degree mutiples to 90. is allowed"
        
        let clockwise size = 
            rotate 90. size

        let ofPageSize (pageSize: PageSize) =
            let w = pageSize.GetWidthF()
            let h = pageSize.GetHeightF()
            create w h

        let tryFindMinSizeBiggerThanFsSizeContinuously (fsSize: FsSize) (getWidth: 'T -> float) (getHeight: 'T -> float) (sizes:'T list) =
            sizes 
            |> List.filter (fun size2 ->
                let wd = getWidth size2 - fsSize.Width
                let hd = getHeight size2 - fsSize.Height
                wd > 0. && hd > 0.
            )
            |> function 
                | [] -> None 
                | filteredSizes -> 
                    filteredSizes |> List.minBy (fun size -> getWidth size * getHeight size) |> Some
        let A4 = ofPageSize PageSize.A4
        let A0 = ofPageSize PageSize.A0


    type FsSizeWithUnit<[<Measure>] 'unit> =
        {
            _Tag: string
            Width: float<'unit>
            Height: float<'unit>
        }


    type FsSizeOfMM = FsSizeWithUnit<mm>

    [<RequireQualifiedAccess>]
    module FsSizeOfMM =
        let ofFsSize (size: FsSize) : FsSizeOfMM =
            {
                _Tag = ""
                Height = toMM size.Height
                Width = toMM size.Width
            }

        let ofRect rect =
            FsSize.ofRect rect
            |> ofFsSize

        let toFsSize (size: FsSizeOfMM) : FsSize =
            {
                Height = toInche size.Height
                Width = toInche size.Width
            }


    [<RequireQualifiedAccess>]
    module PageSize =
        let ofFsSize (fsSize: FsSize) =
            PageSize(float32 fsSize.Width,float32 fsSize.Height)

    [<RequireQualifiedAccess>]
    module PdfPage =
        let setPageBox (rect: Rectangle) (kind: PageBoxKind) (page: PdfPage)  = 
            match kind with
            | PageBoxKind.AllBox -> page |> PdfPage.setMediaBox rect |> PdfPage.setCropBox rect |> PdfPage.setArtBox rect |> PdfPage.setTrimBox rect |> PdfPage.setBleedBox rect
            | PageBoxKind.TrimBox -> page |> PdfPage.setTrimBox rect
            | _ -> failwith "Not implemented"
        
        let getBBox (page: PdfPage) =
            page.GetBBox()

        let getPageBox kind (page: PdfPage) =
            match kind with 
            | PageBoxKind.CropBox -> 
                page.GetCropBox()
            | PageBoxKind.TrimBox ->
                page.GetTrimBox()
            | _ ->
                failwith "Not implemented"

        let increaseHeight (length: float) (page: PdfPage) =
            let rect = page.GetBBox() |> Rectangle.increaseHeight length
            setPageBox rect PageBoxKind.AllBox page 

    [<RequireQualifiedAccess>]
    type Rotation =
        | None
        | Clockwise
        | Counterclockwise
        | R180

    [<RequireQualifiedAccess>]
    module Rotation =
        let isRotated = function
                | Rotation.Clockwise | Rotation.Counterclockwise -> true
                | Rotation.R180 | Rotation.None -> false

        let getDegree = function
            | Rotation.Clockwise  -> 90.
            | Rotation.Counterclockwise -> -90.
            | Rotation.R180 -> 180.
            | Rotation.None -> 0.

    [<RequireQualifiedAccess>]
    type Orientation =
        | LeftTop of float * float
        | LeftBottom of float * float
        | TopRight of float * float
        | Bottom of float * float
        | Center

    [<RequireQualifiedAccess>]
    module Orientation =
        open iText.Layout.Properties
        let getPoint (bbox: Rectangle) (orientation: Orientation) =
            match orientation with 
            | Orientation.LeftBottom (x,y) ->
                bbox.GetXF() + x,bbox.GetYF() + y
            | Orientation.Bottom (x,y) ->
                bbox.GetXF() + x + bbox.GetWidthF() / 2.,bbox.GetYF() + y
            | Orientation.LeftTop (x,y) ->
                bbox.GetXF() + x,bbox.GetTopF() + y
            | Orientation.Center ->
                bbox.GetXF() + bbox.GetWidthF() / 2.,bbox.GetYF() + bbox.GetHeightF() /2.
            | Orientation.TopRight (x,y) ->
                bbox.GetRightF() + x,bbox.GetTopF() + y

        let getPdfAlignment (orientation: Orientation) (rotation: Rotation option) =
            match rotation with 
            | None ->
                match orientation with 
                | Orientation.LeftBottom (x,y) ->
                    TextAlignment.LEFT,VerticalAlignment.BOTTOM
                | Orientation.Bottom (x,y) ->
                    TextAlignment.CENTER,VerticalAlignment.BOTTOM
                | Orientation.LeftTop (x,y) ->
                    TextAlignment.LEFT,VerticalAlignment.TOP
                | Orientation.Center ->
                    TextAlignment.CENTER,VerticalAlignment.MIDDLE
                | Orientation.TopRight (x,y) ->
                    TextAlignment.RIGHT,VerticalAlignment.TOP

            | Some (Rotation.Counterclockwise) ->
                match orientation with 
                | Orientation.TopRight (x,y) ->
                    TextAlignment.LEFT,VerticalAlignment.TOP
                | _ -> failwith "Not implemented"
            | _ -> failwith "Not implemented"

    type Position =
        {
            PageBoxKind: PageBoxKind
            Orientation: Orientation
        }

    [<RequireQualifiedAccess>]
    module Position =
        let getPdfAlignment (pos:Position) =
            Orientation.getPdfAlignment pos.Orientation
        let getPageBox page (pos:Position) =
            PdfPage.getPageBox pos.PageBoxKind page

        let getPoint page (pos:Position) =
            let rect = getPageBox page pos
            Orientation.getPoint rect pos.Orientation

        let createSimple orientation =
            {
                PageBoxKind = PageBoxKind.CropBox
                Orientation = orientation
            }

    [<RequireQualifiedAccess>]
    module Canvas = 
        open System
        open iText.Kernel.Colors

        let inline (!?) s = Nullable(s)

        let addTextToRectWithCustomAlign horizonal vertical (orientation: Orientation) (text: string) (font:PdfFont) (fontSize: float) (fontColor: Color) (canvas: Canvas) = 
            let (x,y) =
                let bbox = canvas.GetRootArea()
                Orientation.getPoint bbox orientation

            canvas.SetFont(font).SetFontColor(fontColor).SetFontSize(float32 fontSize).ShowTextAligned(text,float32 x,float32 y,!? horizonal,!? vertical,0.f) |> ignore


        let addTextToRect (orientation: Orientation) (text: string) (font:PdfFont) (fontSize: float) fontColor (canvas: Canvas) = 
            let (horizonal,vertical) =
                Orientation.getPdfAlignment orientation None
            addTextToRectWithCustomAlign horizonal vertical orientation text font fontSize fontColor canvas

        let addColoredTextToRectWithCustomAlian horizonal vertical (orientation: Orientation) (texts: Text list) (font:PdfFont) (fontSize: float) (canvas: Canvas) = 
            let (x,y) =
                let bbox = canvas.GetRootArea()
                Orientation.getPoint bbox orientation
            let p = new Paragraph()
            texts |> List.iter (fun t -> p.Add(t) |> ignore)
            canvas.SetFont(font).SetFontSize(float32 fontSize).ShowTextAligned(p,float32 x,float32 y,!? horizonal,!? vertical) |> ignore

        let addColoredTextToRect (orientation: Orientation) = 
            let (horizonal,vertical) =
                Orientation.getPdfAlignment orientation None
            addColoredTextToRectWithCustomAlian horizonal vertical orientation

        let addTextToRectWithRotation (orientation: Orientation) (text: string) (font:PdfFont) (fontSize: float) (fontColor: Color) (rotation:Rotation) (canvas: Canvas) = 
            let (x,y),(horizonal,vertical) =
                let bbox = canvas.GetRootArea()
                Orientation.getPoint bbox orientation,Orientation.getPdfAlignment orientation (Some rotation)

            let angle = radians (Rotation.getDegree rotation)
            canvas.SetFont(font).SetFontColor(fontColor).SetFontSize(float32 fontSize).ShowTextAligned(text,float32 x,float32 y,!? horizonal,!? vertical,float32 angle) |> ignore

    type PdfCanvas with
        member this.SetStrokeColorToRegistation() =
            let doc = this.GetDocument()
            this.SetStrokeColor(Color.registion doc)

    type SplitterDocument = 
        {
            Src: PdfDocument
            Dest: PdfDocument
        }

    [<RequireQualifiedAccessAttribute>]
    module SplitterDocument = 
        let create (src: string) (dest: string) = 
            {
                Src = new PdfDocument(new PdfReader(src))
                Dest = new PdfDocument(new PdfWriter(dest))
            }

        let createSimple (src: string) (dest: string) =
            create src dest

        let private close (doc: SplitterDocument) = 
            doc.Src.Close()
            doc.Dest.Close()

        let clear (src: string) (dest: string) (doc: SplitterDocument) =
            close doc
            draft src dest

    type Cropmark = 
        {
            Length: float
            Distance: float
            Width: float
        }

    [<RequireQualifiedAccess>]
    module Cropmark =
        let simple = 
            {
                Length = toInche 3.8
                Distance = toInche 3.2
                Width = toInche 0.2
            }

        let isEmpty cropmark =
            cropmark.Length = 0. 
            || cropmark.Width = 0.



    [<RequireQualifiedAccess>]
    type Background =
        | Size of FsSize
        | File of string
        | None

    [<RequireQualifiedAccess>]
    module Background =
        let asSize  = function
            | Background.Size size -> Some size
            | _ -> None
 
    [<RequireQualifiedAccess>]
    type FlipWay =
        | HFlip
        | VFlip
        | FB


    type ImposingArgumentBase<'desiredSize,'background> =
        {
            ColNums: int list
            RowNum: int
            Cropmark: Cropmark option
            HSpaces: float list
            VSpaces: float list
            Margin: Margin
            UseBleed: bool
            Background: 'background
            RotateXObjectDegree: Rotation
            DesiredSize: 'desiredSize
            IsRepeated: bool
        }

    [<RequireQualifiedAccess>]
    module ImposingArgumentBase =
        let createDummy desiredSize background : ImposingArgumentBase<_,_> =
            {
                ColNums = [0]
                RowNum = 0
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.createSimple 0.
                UseBleed = false
                Background = background
                RotateXObjectDegree = Rotation.None
                DesiredSize = desiredSize
                IsRepeated = false
            }
        let map desiredSize background arg =
            {
                ColNums             =   arg.ColNums             
                RowNum              =   arg.RowNum 
                Cropmark            =   arg.Cropmark 
                HSpaces             =   arg.HSpaces 
                VSpaces             =   arg.VSpaces 
                Margin              =   arg.Margin 
                UseBleed            =   arg.UseBleed 
                Background          =   background
                RotateXObjectDegree =   arg.RotateXObjectDegree 
                DesiredSize         =   desiredSize 
                IsRepeated          =   arg.IsRepeated 
            }
            

    type ImposingArgument = ImposingArgumentBase<FsSize option,Background>

    [<RequireQualifiedAccess>]
    module ImposingArgument =
        open Atrous

        let dummy : ImposingArgument = ImposingArgumentBase.createDummy None Background.None

        let clockwiseBackground (arg:ImposingArgument) =
            let back = 
                match arg.Background with 
                    | Background.Size size -> FsSize.clockwise size |> Background.Size
                    | Background.File _ -> Logger.notImplemented()
                    | Background.None -> Background.None

            { arg with Background = back }

    [<RequireQualifiedAccess>]
    type ImposerPosition =
        | FirstANDLast
        | First
        | Last
        | Middle

    [<RequireQualifiedAccess>]
    module CellPosition =
        let create (rowLength,index) = 
            match (rowLength,index) with
            | 1,0 -> ImposerPosition.FirstANDLast
            | l,i ->
                if l > 1 then
                    if i = 0 then ImposerPosition.First
                    elif i = l - 1 then ImposerPosition.Last
                    else ImposerPosition.Middle
                else failwith "Not implemented"
    
    type ImposerCell = 
        {
            Page: PdfPage
            Size: FsSize
            XObject: PdfFormXObject
            X: float
            Bleed: bool
            Position: ImposerPosition
            Degree: float
            Space: float
            PreSpace: float
        }

    type RowDrawingArgument =
        {
            Y: float
            Position: ImposerPosition
            PreSpace: float
            Space: float
        }

    [<RequireQualifiedAccess>]
    module ImposerCell =

        let addToCanvas rowArg (u: ImposerCell) (canvas: PdfCanvas) = 
            let y,rowPosition = rowArg.Y,rowArg.Position
            let xobject = u.XObject
            let bbox = 
                if u.Bleed then
                    xobject 
                    |> PdfFormXObject.tryGetTrimBox
                    |> function
                        | Some rect -> rect
                        | None -> xobject |> PdfFormXObject.getBBox
                else
                    u.Page.GetBBox()

            let tf = new AffineTransform()

            let degree = u.Degree
            if degree % 90. = 0. then 

                let rotate =
                    let x,y = bbox.GetXF(),bbox.GetYF()
                    tf.Rotate(radians -degree,x,y)

                let rect = 
                    let rect = tf.TransformRectangle(bbox)
                    let scaleX,scaleY =
                        u.Size.Width / rect.GetWidthF(),
                        u.Size.Height / rect.GetHeightF()
                    //let scaleY =
                    //    Math.Round(scaleY,2)
                    if degree % 180. = 0. then
                        tf.Scale(scaleX,scaleY)
                    else 
                        tf.Scale(scaleY,scaleX)
                    tf.TransformRectangle(bbox)
                


                let setClippingBox xobject =
                    let horizonalMargin (margin: Margin) =
                        let w = u.Space / 2.
                        let l = u.PreSpace / 2.
                        match u.Position with 
                        | ImposerPosition.First ->
                            { margin with Right = w }
                        | ImposerPosition.Middle ->
                            { margin with
                                Right = w
                                Left = l }
                        | ImposerPosition.Last ->
                            { margin with
                                Left = l }
                        | ImposerPosition.FirstANDLast ->
                            margin

                    let verticalMargin (margin: Margin) =
                        let w = rowArg.Space / 2.
                        let l = rowArg.PreSpace / 2.
                        match rowPosition with 
                        | ImposerPosition.First ->
                            { margin with Bottom = w }
                        | ImposerPosition.Middle ->
                            { margin with
                                Bottom = w
                                Top = l }
                        | ImposerPosition.Last ->
                            { margin with
                                Top = l }
                        | ImposerPosition.FirstANDLast ->
                            margin
                    let rect = 
                        let margin =
                            if u.Bleed then
                                let originBox = xobject |> PdfFormXObject.getBBox |> tf.TransformRectangle
                                {
                                    Left = rect.GetXF() - originBox.GetXF()
                                    Right = -rect.GetRightF() + originBox.GetRightF() 
                                    Top = -rect.GetTopF() + originBox.GetTopF()
                                    Bottom = rect.GetBottomF() - originBox.GetBottomF()
                                }
                                |> horizonalMargin
                                |> verticalMargin
                            else Margin.createSimple 0.
                        rect |> Rectangle.applyMargin margin |> tf.InverseTransformRectangle
                    xobject |> PdfFormXObject.setBBox rect
                
                
                let matrix = Array.create 6 0.f
                tf.GetMatrix(matrix)

                canvas.AddXObject
                    (setClippingBox xobject,matrix.[0],
                        matrix.[1],matrix.[2],matrix.[3],
                        matrix.[4] - rect.GetX() + float32 u.X, 
                        matrix.[5] - rect.GetY() + float32 y - rect.GetHeight()) 
                    |> ignore
            else 
                failwithf "Incurrent degree %f" degree

        let draw (canvas: PdfCanvas) rowArg (areas:Rectangle list) (cropmark: Cropmark option) (u: ImposerCell) =
            addToCanvas rowArg u canvas
            let x = u.X
            let y,rowPosition = rowArg.Y,rowArg.Position
            let height = u.Size.Height
            let width = u.Size.Width
            match cropmark with 
            | Some cropmark ->
                let areas = areas |> List.map (Rectangle.applyMargin (Margin.createSimple (cropmark.Distance - tolerance)))
                let length = cropmark.Length
                let distance = cropmark.Distance

                let lines = 
                    [
                        x, y-distance, x, y-distance-length
                        x-distance, y, x-distance-length, y        
                        x-distance, y+height, x-distance-length, y+height        
                        x, y+distance+height, x, y+distance+length+height    
                        x+width, y-distance, x+width, y-distance-length
                        x+distance+width, y, x+distance+length+width, y        
                        x+distance+width, y+height, x+distance+length+width, y+height        
                        x+width, y+distance+height, x+width, y+distance+length+height   
                    ]

                lines 
                |> List.filter (fun l -> List.forall (Rectangle.straightLineoutsideBox l) areas)
                |> List.iter (fun (x1,y1,x2,y2) ->
                    canvas
                        .MoveTo(x1,y1 - height)
                        .LineTo(x2,y2 - height)
                        .Stroke()
                        |> ignore
                )

            | None -> ()


    type ImposerRow = 
        {
            Width: float
            Height: float
            Y: float
            Units: ImposerCell list
            Position: ImposerPosition
            Space: float
            PreSpace: float
        }
        
    [<RequireQualifiedAccess>]
    module ImposerRow = 
        let draw canvas y areas cropmark (r: ImposerRow) = 
            let arg =
                {
                    Y = r.Y
                    Position = r.Position
                    Space = r.Space
                    PreSpace = r.PreSpace
                }
            r.Units 
            |> List.iter (fun u -> ImposerCell.draw canvas arg areas cropmark u)


    type ImposerTable =
        {
            Rows: ImposerRow list
            Height: float
            Width: float
            Cropmark: Cropmark option
            DestDoc: PdfDocument
            Margin: Margin
            Background: Background
        }

    [<RequireQualifiedAccess>]
    module ImposerTable = 
        let private autofitBBox (p: ImposerTable) page =
            let rect = Rectangle.create 0 (-p.Height) p.Width p.Height |> Rectangle.applyMargin (p.Margin)
            page |> PdfPage.setPageBox rect PageBoxKind.AllBox |> ignore

        [<AutoOpen>]
        module private Cache =
            let bks = new Dictionary<string,PdfDocument * PdfFormXObject * Rectangle>()

        let private addBackground (path: string) (p: ImposerTable) (page: PdfPage) (canvas: PdfCanvas) =
            let margin  = p.Margin
            let autofit() = 
                let dest = page.GetDocument()
                let xobject,srcBBox =
                    bks.ContainsKey path 
                    |> function
                        | true -> 
                            let _,xobject,bbox = bks.[path]
                            xobject,bbox
                        | false -> 
                            let doc = new PdfDocument(new PdfReader(path))
                            let page = doc.GetFirstPage()
                            let bbox = page.GetBBox()
                            let xobject = page.CopyAsFormXObject(dest)
                            bks.Add(path,(doc,xobject,bbox))
                            xobject,bbox
                let height = srcBBox.GetHeightF()
                let width = srcBBox.GetWidthF()
                let x = -margin.Left
                let y = 
                    let y = (p.Height) 
                    let offset = height - p.Height - margin.Top
                    -y - offset
                let rect = Rectangle.create x y width height
                page |> PdfPage.setPageBox rect PageBoxKind.AllBox |> ignore
                xobject,x,y

            let xobject,x,y = autofit()

            canvas
            |> PdfCanvas.addXObject xobject x y
            |> ignore

        let draw (p: ImposerTable) = 
            let areas = 
                p.Rows 
                |> List.collect (fun r -> 
                    r.Units
                    |> List.map (fun u -> 
                        let x = u.X
                        let y = r.Y
                        let width = u.Size.Width
                        let height = u.Size.Height
                        Rectangle.create x y width height
                    ) 
                ) 
            let page = p.DestDoc.AddNewPage()
            let canvas = new PdfCanvas(page)
            match p.Background with 
            | Background.File path -> 
                addBackground path p page canvas
            | Background.Size _ ->  autofitBBox p page
            | Background.None -> autofitBBox p page

            PdfCanvas.useCanvas canvas (fun _ ->

                match p.Cropmark with 
                | Some cropmark -> canvas.SetStrokeColorToRegistation().SetLineWidth(cropmark.Width) |> ignore
                | None -> ()

                p.Rows
                |> List.iter (fun r -> ImposerRow.draw canvas r.Y areas p.Cropmark r)
            )

        let fullWidth (p: ImposerTable) =
            p.Width + p.Margin.Left + p.Margin.Right

        let fullHeight (p: ImposerTable) =
            p.Height + p.Margin.Top + p.Margin.Bottom
    
    type FlowState<'userState> =
        {
            UserState: 'userState
            Tables: ImposerTable list
            LastManipulatesInfos: AbstractRenderInfo list
        }

    [<RequireQualifiedAccess>]
    module FlowState =
        let mapUserState mapping flowState =
            {
                UserState = mapping flowState.UserState
                Tables = flowState.Tables
                LastManipulatesInfos = flowState.LastManipulatesInfos
            }

    type FlowModel<'userState> =
        {
            Path: string
            FlowState: FlowState<'userState>
        }
    [<RequireQualifiedAccess>] 
    module FlowModel =
        let mapUserState (mapping:'userState -> 'newUserState) (flowModel: FlowModel<'userState>) =
            {
                Path = flowModel.Path
                FlowState = FlowState.mapUserState mapping flowModel.FlowState
            }

    type ManipulateArgument<'userState> =
        {
            TotalPageNum: int
            Page: PdfPage
            PageNum: int
            Parser: PdfDocumentContentParser
            Select: AbstractRenderInfo -> bool
            ImposerTable: ImposerTable option
            FlowState: FlowState<'userState>
        }

    [<RequireQualifiedAccess>]
    module ManipulateArgument =
        let useFont fontGen arg f =
            let page = arg.Page 
            let doc = page.GetDocument() :?> PdfDocumentWithResources
            PdfDocumentWithResources.useFont fontGen doc f

        let mapUserState mapping (arg: ManipulateArgument<_>) =
            {
                TotalPageNum = arg.TotalPageNum
                Page = arg.Page
                PageNum = arg.PageNum
                Parser = arg.Parser
                Select = arg.Select
                ImposerTable = arg.ImposerTable
                FlowState = FlowState.mapUserState mapping arg.FlowState
            }

    type Manipulate<'userState> = ManipulateArgument<'userState> -> FlowState<'userState>

    type Reuse<'userState> = FlowModel<'userState> -> list<FlowModel<'userState>>

    [<RequireQualifiedAccess>]
    type Flow<'userState> = 
        | Reuse of Reuse<'userState>
        | Manipulates of Manipulate<'userState> list
        | Composite of (Flow<'userState> list)
        | Manipulate of Manipulate<'userState>
        | FromState of (FlowState<'userState> -> Flow<'userState>)
        | TransformUserState of ('userState -> 'userState)
        | Rename of (('userState * string) -> string)
        | Iac of (string -> unit)
        | Read of (PdfDocument -> 'userState -> 'userState)
    
    [<RequireQualifiedAccess>]
    module Flow = 
        let asManipulate flow =
            match flow with 
            | Flow.Manipulate m -> Some m
            | _ -> None

        let asReuse flow =
            match flow with 
            | Flow.Reuse m -> Some m
            | _ -> None
            

    [<AutoOpen>]
    module Extensions =
        [<RequireQualifiedAccess>]
        module PdfDocument = 

            let useAsReader (path: string) (usingCase: PdfDocument -> 'result) =
                let doc = new PdfDocument(new PdfReader(path))
                let result = usingCase doc
                doc.Close()
                result

            let create (src: string) (dest: string) = 
                new PdfDocument(new PdfReader(src),new PdfWriter(dest))

            let getAllPages (doc: PdfDocument) = 
                [
                    for i = 1 to doc.GetNumberOfPages() do
                        yield doc.GetPage(i)
                ]

            let internal manipulateForEachPageWithFoldingStates manipulate (flowState: FlowState<_>)(doc: PdfDocument) = 
                let parser = new PdfDocumentContentParser(doc)
                let pages = getAllPages doc

                let tables = flowState.Tables
                if tables.Length = pages.Length then
                    pages |> List.zip tables |> List.indexed |> List.fold (fun flowState zip ->
                        let i,(table,page) = zip
                        let pageNum = i + 1
                        {
                            Page = page
                            PageNum = pageNum
                            Parser = parser
                            ImposerTable = Some table
                            FlowState = flowState
                            TotalPageNum = doc.GetNumberOfPages()
                            Select = fun _ -> true
                        } |> manipulate
                    ) flowState
                else 
                    pages |> List.indexed |> List.fold (fun flowState zip ->
                        let pageNum,page = zip
                        let pageNum = pageNum + 1
                        {
                            Page = page
                            PageNum = pageNum
                            Parser = parser
                            ImposerTable = None
                            FlowState = flowState
                            TotalPageNum = doc.GetNumberOfPages()
                            Select = fun _ -> true
                        } |> manipulate
                    ) flowState

    let manipulate (f: ManipulateArgument<_> -> FlowState<_>) = 
        f |> Flow.Manipulate

    let manipulates (fs: (ManipulateArgument<_> -> FlowState<_>) list) = 
        fs |> List.map manipulate |> List.choose Flow.asManipulate |> Flow.Manipulates

    let manipulatesStepByStep (fs: (ManipulateArgument<_> -> FlowState<_>) list) = 
        fs |> List.map manipulate |> Flow.Composite

    let forPageNums (prediate: int -> bool) (manipulate: ManipulateArgument<_> -> FlowState<_>) =
        fun arg ->
            if prediate arg.PageNum then
                manipulate arg
            else arg.FlowState

    let inPages (transform: int list -> int list) (manipulate: int list -> int -> int -> (ManipulateArgument<_> -> FlowState<_>)) =
        fun (arg: ManipulateArgument<_>) ->
            let totalNumber = arg.TotalPageNum
            let ns = [1..totalNumber] |> transform
            ns |> List.tryFindIndex ((=) arg.PageNum)
            |> function 
                | Some index ->
                    (manipulate ns index arg.PageNum) arg
                | None -> arg.FlowState

