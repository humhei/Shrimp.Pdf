namespace Shrimp.Pdfargets
open iText.Kernel.Pdf.Canvas.Parser.Data

module Actions = 
    [<RequireQualifiedAccess>]
    module Read =
        open Shrimp.Pdf
        open System
        open Types
        open Shrimp.Entities.Types
        open Shrimp.Pdf.Extensions
        open Shrimp.Pdf.Filters
        open Shrimp.Pdf.Colors
        open Shrimp.Pdf.DocReader
        open Shrimp.Pdf.Select
        open Shrimp.Extensions
        open Shrimp.Pdf.Targets

        let private readAndSetUserState reader setUserState =
            Flow.Read (fun doc state ->
                let infos = reader doc
                setUserState infos state
            )

        let subRects (filters: list<AnalyticSubpath -> bool>) =
            readAndSetUserState (fun doc ->
                let subPathInfos = PdfDocument.readAnalyticSubpaths filters doc
                subPathInfos |> List.map (List.map AnalyticSubpath.getBound)
            )

        let private getSingleText filters doc =
            PdfDocument.readTexts filters doc
            |> List.concat
            |> List.distinctBy (fun (tr) -> tr.GetText()) |> List.exactlyOne

        let private size getFsSize filters = 
            readAndSetUserState (fun doc ->
                let info = getSingleText filters doc 
                let fsSize = getFsSize (info.GetText())
                info,fsSize
            )
        

        let btwSize setUserState = 
            let filters = [Text.fill Colors.Btw.size <&> Text.fparsec Btw.sizeParser]
            size FsSize.fromStringBtw filters setUserState
    
        let aiSize setUserState = 
            let filters = [ Text.fparsec AI.sizeParser ]
            size FsSize.fromStringAI filters setUserState

        //let extractFirstPageToBrandPage : Flow<ReportInfo<_,_>> = 
        //    Flow.Read (fun doc state ->
        //        let page = doc.GetFirstPage()
        //        let tmp = Path.GetTempFileName()
        //        let writer = new PdfDocument(new PdfWriter(tmp))
        //        page |> PdfPage.addToDoc(writer)
        //        writer.Close()
        //        [fixPosition()] |> run tmp |> ignore
        //        { state with BrandPage = tmp } 
        //    )
    
        let colors (bleed: Bleed) =
            let getPageBox = fun page -> 
                match bleed with 
                | Bleed.Some margin -> 
                    PdfPage.getTrimBox page |> Rectangle.applyMargin margin
                | Bleed.None -> PdfPage.getBBox page

            readAndSetUserState (fun doc ->
                let colors = 
                    PdfDocument.colorsCrossOrInBox getPageBox (Margin.empty) doc |> List.concat |> Colors.distinct |> List.ofSeq
                colors
            )


        let desiredColor (bleed: Bleed) set =
            colors bleed (fun colors userState ->
                let desiredColor = DesiredColor.ofColors colors
                set desiredColor userState 
            )


    module FromState =
        open Shrimp.Pdf.Types
        open Shrimp.Pdf.Reuses
        open Shrimp.Pdf
        open Shrimp.Pdf.Extensions
        open Types

        let imposeWithStateOfArgs : Flow<ReportInfo<_,_>> = 
            Flow.FromState (fun state ->
                let arg = state.UserState.Arg
                impose (ExplicitImposingArgument.toImplicit arg)
            )

        let resizeWithStateOfSize : Flow<ReportInfo<_,_>> = 
            Flow.FromState (fun state ->
                let size = state.UserState.Arg.DesiredSize
                resize size
            )

    module Manipulates =
        open Shrimp.Pdf.Types
        open Shrimp.Pdf.Manipulates
        open Shrimp.Pdf.Operators
        open Shrimp.Pdf.Targets
        open Shrimp.Pdf
        open Shrimp.Pdf.Extensions
        open Shrimp.Pdf.Colors
        open Shrimp.Pdf.Filters
        open Shrimp.Pdf.Select
        open Shrimp.Pdf.PageReader
        open Shrimp.Entities.Types
        open Shrimp.Utils
        open iText.Kernel.Pdf.Canvas
        open Shrimp.Pdf.DocReader
        open Shrimp.Pdf.Targets.Types

        let removeCuttingLineAI arg =
            arg |> removePathWithStrokeColors Colors.AI.cuttingLine

        let retatinCuttingLineAI arg =
            arg |> retainPathWithStrokeColors Colors.AI.cuttingLineWithRegistion

        let fixImpressLineWithColors colors modify arg =
            let entityFilter = 
                [
                    Path.dash
                    Path.stroke colors
                ]

            let position = 
                let getYMax (prInfo: PathRenderInfo) =
                    prInfo 
                    |> PathRenderInfo.toLiteralPoints 
                    |> List.ofSeq
                    |> List.map (fun p -> p.y)
                    |> List.max

                let infos = ManipulateArgument.readPaths entityFilter arg |> List.ofSeq

                let ys =
                    infos
                    |> List.choose AbstractRenderInfo.asPathRenderInfo
                    |> List.map getYMax
                fun (prInfo: PathRenderInfo) ->
                    let y2 = prInfo |> getYMax
                    List.contains y2 ys

            arg 
            |> fixWithChoice
                [
                    path
                        [Path.stroke colors <&> position]
                    text [Text.fill [Color._registion]]
                    path [Path.stroke [Color._registion]]
                ]
                modify


        let fixImpressLine modify arg =
            fixImpressLineWithColors Colors.cuttingLine modify arg

        let removeImpressLine arg =
            fixImpressLine Modify.remove arg

        let fixImpressLineAI modify arg =
            fixImpressLineWithColors Colors.AI.cuttingLine modify arg

        let retainImpressLineAI arg =
            fixImpressLineAI Modify.retain arg


        let addSeamMark arg =
            let ms = 
                [
                    addLine (Position.createSimple (Orientation.Bottom (0.,toInche 0.))) (Position.createSimple (Orientation.Bottom (0.,toInche 3.3)))
                    addLine (Position.createSimple (Orientation.Bottom (toInche -3.,toInche 3.3))) (Position.createSimple (Orientation.Bottom (toInche 3.,toInche 3.3)))
                ] 
            batch ms arg 


        let [<Literal>] private seamTextSize = 9.
        let [<Literal>] private seamTextBotm = 0.
        let private seamColorPos = toInche 10.

        let private textInfo text (arg: ManipulateArgument<_>) =
            let doc = arg.Page.GetDocument()
            TextArgument.createWith (fun arg ->
                { arg with 
                    Text = text
                    FontColor = (Color.registion doc)
                    FontSize = seamTextSize }
            )
            
        let addPageNumSignatureToMiddle fontColor (arg: ManipulateArgument<_>) =
            let page = arg.Page
            let bbox = PdfPage.getBBox page
            let text = arg.PageNum.ToString()
            addTextToRectWithScale text FontGen.HelveticaBold fontColor 0.35 bbox arg

        let addSeamText text (arg: ManipulateArgument<_>) =
            let pos = Position.createSimple (Orientation.Bottom(toInche -3, seamTextBotm))
            arg |> addText pos (textInfo text arg)

        let addTextToTopRight text (arg: ManipulateArgument<_>) =
            let pos = Position.createSimple (Orientation.TopRight(toInche 0.,toInche -10.))
            arg |> addTextWithRotation pos Rotation.Counterclockwise (textInfo text arg)

        let addCMYKMarker (arg: ManipulateArgument<_>) = 
            let pos = Position.createSimple (Orientation.Bottom (seamColorPos,toInche 1))
            let page = arg.Page
            let canvas = new PdfCanvas(page)
            let x,y = pos |> Position.getPoint page
            canvas.AddInDirectXObject(Marks.cmyk,x,y) |> ignore
            arg.FlowState

        let addSeperationColor (kys: ColorCard list) arg = 
            let pos = Position.createSimple (Orientation.Bottom (seamColorPos,seamTextBotm))
            let texts = kys |> List.map ColorCard.toColoredText
            addColoredText pos texts FontGen.heiti seamTextSize arg


        let addTextToLeftTop (text: string) arg =
            arg |> addText (Position.createSimple (Orientation.LeftTop (toInche 8.,toInche -2.))) (textInfo text arg)

        let addTextToLeftBottom (text: string) arg =
            addText (Position.createSimple (Orientation.LeftBottom (toInche 42.,toInche 2.))) (textInfo text arg)

        let bleedOut arg = (bleedOut (Margin.createSimple (toInche 1.5))) arg
    
        [<RequireQualifiedAccess>]
        module Bleed = 
            let bleedOutToStrokeBlue = function
                | Bleed.None -> trimToStrokeBlue
                | Bleed.Some _ -> batch [trimToStrokeBlue;bleedOut]

            let bleedOutToStrokeCyanOrMagenta = function
                | Bleed.None -> trimToStrokeCyanOrMegenta
                | Bleed.Some _ -> batch [trimToStrokeCyanOrMegenta;bleedOut]

    module Reuses =
        open Shrimp.Entities.Types
        open Shrimp.Pdf.Targets.Types
        open Shrimp.Pdf.Types
        open Shrimp.Pdf.Reuses
        open FileSystem
        open iText.Kernel.Pdf
        open Fake.IO.FileSystemOperators
        open Shrimp.Pdf.Extensions

        let imposeExplicit (arg: ExplicitImposingArgument) =
            ExplicitImposingArgument.toImplicit arg |> impose

        let assignPagesToOrders (p: Product) root targetName (tables: TypedImposerDataTable list) setUserState : Flow<_> =
            multiple (
                fun srcDoc (model: FlowModel<_>) ->
                    let pages = PdfDocument.getAllPages srcDoc
                    let rec assign (tables: TypedImposerDataTable list) (pages: PdfPage list) accum = 
                        match tables with 
                        | table::t ->
                            let dir = root </> targetName </> table.Name
                            ensureDir dir
                            let dest = dir </> p.Name + ".pdf"
                            let destDoc = 
                                let writer = new PdfWriter(dest)
                                new PdfDocument(writer)

                            let datas = table.Rows
                            pages.[0 .. datas.Length - 1] |> List.iter (PdfPage.addToDoc destDoc)
                            let state = 
                                let state = model.FlowState
                                { state with
                                    UserState = 
                                        setUserState table state.UserState
                                }
                            assign t pages.[datas.Length ..] (({ model with Path = dest; FlowState = state },destDoc) :: accum)
                        | [] -> accum
                    assign tables pages []
            )

        let assignPagesToOrdersOfTarm (p:Product) tarm targetName (tables: TypedImposerDataTable list) setUserState : Flow<_> =
            let root = TargetModel.tagDir tarm
            assignPagesToOrders (p:Product) root targetName tables setUserState
    [<AutoOpen>]
    module Operators =

        open System.IO
        open Shrimp.Pdf.Types
        open Shrimp.Pdf.Operators
        open Manipulates
        open Types

        let createSampleDocumentByPageNum totalPageNumber pageSize =
            let path = Path.GetTempFileName()
            createPdf path (List.replicate totalPageNumber pageSize)
            [
                manipulates 
                    [
                        fun arg ->
                            let doc = arg.Page.GetDocument()
                            let color = Color.pageNum doc
                            addPageNumSignatureToMiddle color arg
                    ]
            ]
            |> simplePreflight path
            path

        let createSampleDocument (tpTables: TypedImposerDataTable list) pageSize =
            let totalPageNumber = 
                tpTables |> List.collect (fun tpTable -> tpTable.Rows) |> List.length

            createSampleDocumentByPageNum totalPageNumber pageSize
