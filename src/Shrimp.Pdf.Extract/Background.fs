// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open Shrimp.Pdf.SlimFlow
open Shrimp.Pdf.Extract
open Shrimp.LiteDB
open System.IO
#nowarn "0104"
open Shrimp.Pdf.Constants
open Shrimp.Pdf.Extensions
open Akkling
open Fake.IO

#nowarn "0104"
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open Shrimp.Pdf.Colors
open iText.IO.Image
open System.Collections.Concurrent
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Geom

[<AutoOpen>]
module _Extract_BackgroundOrForeground =
    
    let private applyName name (text: string) =
        match name with 
        | String.EqualIC UNTITLED ->
            text

        | name ->
            let name = 
                match name.LeftOf("-") with 
                | Some name -> name
                | None -> name

            sprintf "%s.%s" name text


    [<AutoOpen>]
    module private _BackgroundFileCache =
        let backgroundCache = 
            new LightWeightFileInfoDictionary<PdfFile * string, PdfFile, PdfPath * string>(
                cacheFileMapping = (fun (m, name) -> 
                    let dir = Path.getDirectory m.Path </> ".bk"
                    Directory.ensure dir
                    let ext = applyName name ".cache"
                    dir </> (Path.GetFileName m.Path)
                    |> Path.changeExtension ext
                ),
                keyFilesGetter = (fun (m, _) -> [m.Path]),
                valueFilesGetter = (fun m -> [m.FileInfo]),
                primaryKeyGetter = (fun (m, name) -> primaryKey (m.PdfPath, name))
            )

    type BackgroundFile with 
        static member Create(pdfFile: PdfFile, addtionalFlow: BackgoundAddtionFlow) =
            let name = addtionalFlow.Name
            let clearedPdfFile = 
                backgroundCache.GetOrAddOrUpdate((pdfFile, name), fun () ->
                    let targetPath =
                        let dir = Path.getDirectory pdfFile.Path </> ".bk"
                        Directory.ensure dir
                        let ext = applyName name "backgroundFile.cleared.pdf"
                        dir </> Path.GetFileName pdfFile.Path
                        |> Path.changeExtension ext


                    let newPdfFile: PdfFile = 
                        let flow = 
                            Flow.Reuse(
                                Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
                            )
                            <+>
                            (
                                match addtionalFlow with 
                                | BackgoundAddtionFlow.Keep -> Flow.dummy() ||>> ignore
                                | BackgoundAddtionFlow.ExtractObjects -> 
                                    Flow.Reuse(
                                        Reuses.ExtractIM(PageSelector.All, Selector.All(fun _ _ -> true))
                                    )
                                | BackgoundAddtionFlow.CustomCase (_, flow) ->  flow
                            )

                        runWithBackup targetPath pdfFile.Path (flow)
                        |> List.exactlyOne_DetailFailingText
                        |> fun flowModel -> flowModel.PdfFile

                    newPdfFile
                )
                |> Async.RunSynchronously

            (pdfFile, clearedPdfFile, Rotation.None)
            |> BackgroundFile



        static member Create(pdfFile: PdfFile) =
            BackgroundFile.Create(pdfFile, BackgoundAddtionFlow.Keep)

        static member Create(path: string) =
            PdfFile path
            |> BackgroundFile.Create

        static member Create(path: string, addtionalFlow) =
            BackgroundFile.Create(PdfFile path, addtionalFlow)

    type Reuses with 
        
        static member private AddBackgroundOrForegroundImage(image: BackgroundImageFile, choice: BackgroundOrForeground, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak, ?pageSelector, ?layerName: BackgroundAddingLayerOptions, ?extGSState: FsExtGState) =
            let xEffect = defaultArg xEffect XEffort.Middle
            let yEffect = defaultArg yEffect YEffort.Middle
            //let addShadowColor pageSize (pdfCanvas: PdfCanvas) =
            //    match shadowColor with 
            //    | None -> pdfCanvas
            //    | Some (pdfFileShadow: NullablePdfCanvasColor) ->
            //        pdfCanvas
            //            .SaveState()
            //            .SetFillColor(pdfFileShadow)
            //            .Rectangle(pageSize)
            //            .Fill()
            //            .RestoreState()


            let pageSelector = defaultArg pageSelector PageSelector.All
            (fun flowModel (doc: SplitDocument) ->
                let imageCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
                let reader = doc.Reader
                let selectedPages = reader.GetPageNumbers(pageSelector)
                PdfDocument.getPages reader
                |> List.mapi (fun i readerPage ->
                    let pageNumber = i + 1
                    let typedPageNumber = PageNumber pageNumber
                    match List.contains (i+1) selectedPages with 
                    | true ->
                        let readerPageBox = readerPage.GetActualBox()

                        let pageSize = 
                            readerPageBox
                            |> PageSize

                        let writerPage = doc.Writer.AddNewPage(pageSize)
                        writerPage.SetPageBoxToPage(readerPage) |> ignore
                        let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

                        writerPage.AddBackgroundOrForegroundImage(
                            i,
                            imageCache,
                            RenewableBackgroundImageFile.BackgroundImageFile (image, BackgroundFile.Create),
                            choice, 
                            addSelf = (fun canvas -> canvas.AddXObjectAbs(readerXObject, 0.f, 0.f) |> ignore<PdfCanvas>),
                            ?shadowColor = shadowColor,
                            xEffect = xEffect,
                            yEffect = yEffect,
                            ?backgroundPositionTweak = backgroundPositionTweak,
                            ?layerName = layerName,
                            ?extGSState = extGSState
                        )
                    | false ->
                        let page = readerPage.CopyTo(doc.Writer)
                        doc.Writer.AddPage(page)
                        |> ignore
                    )
                    |> ignore


                imageCache
                |> List.ofSeq
                |> List.iter(fun m -> 
                    m.Value.Close()
                )

            )
            |> reuse 
                "AddBackgroundOrForeground"
                [
                    "pageSelector" => pageSelector.ToString()
                    "backgroundFile" => image.ToString()
                    "layer" => choice.ToString()
                    "xEffect" => xEffect.ToString()
                    "yEffect" => yEffect.ToString()
                ]

        
        //static member private AddBackgroundOrForegroundImage0(image: BackgroundImageFileValue, choice: BackgroundOrForeground, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak, ?pageSelector, ?layerName: BackgroundAddingLayerOptions, ?extGSState: FsExtGState) =
        //    let xEffect = defaultArg xEffect XEffort.Middle
        //    let yEffect = defaultArg yEffect YEffort.Middle
        //    let addShadowColor pageSize (pdfCanvas: PdfCanvas) =
        //        match shadowColor with 
        //        | None -> pdfCanvas
        //        | Some (pdfFileShadow: NullablePdfCanvasColor) ->
        //            pdfCanvas
        //                .SaveState()
        //                .SetFillColor(pdfFileShadow)
        //                .Rectangle(pageSize)
        //                .Fill()
        //                .RestoreState()


        //    let pageSelector = defaultArg pageSelector PageSelector.All
        //    (fun flowModel (doc: SplitDocument) ->
        //        let imageCache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
        //        let reader = doc.Reader
        //        let selectedPages = reader.GetPageNumbers(pageSelector)
        //        PdfDocument.getPages reader
        //        |> List.mapi (fun i readerPage ->
        //            let pageNumber = i + 1
        //            let typedPageNumber = PageNumber pageNumber
        //            match List.contains (i+1) selectedPages with 
        //            | true ->
        //                let readerPageBox = readerPage.GetActualBox()
        //                let readerXObject = readerPage.CopyAsFormXObject(doc.Writer)

        //                let pageSize = 
        //                    readerPageBox
        //                    |> PageSize

        //                let writerPage = doc.Writer.AddNewPage(pageSize)
        //                writerPage.SetPageBoxToPage(readerPage) |> ignore
        //                let pdfCanvas = new PdfCanvas(writerPage)

        //                let image = 
        //                    let createImage (imageFile: FsFullPath) =
        //                        imageCache.GetOrAdd(imageFile, valueFactory = fun imageFile ->
        //                            match imageFile.Path.ToLower() with 
        //                            | String.EndsWith ".pdf" -> 
        //                                let backgroundFile = BackgroundFile.Create imageFile.Path
        //                                let backgroundInfos =

        //                                    let pageBoxs = BackgroundFile.getPageBoxes backgroundFile
        //                                    let totalPageNumber = pageBoxs.Length
        //                                    let reader = new PdfDocument(new PdfReader(backgroundFile.ClearedPdfFile.Path))
          
        //                                    {|  
        //                                        Close = fun () -> reader.Close()
        //                                        PageBoxAndXObject = 
        //                                            match image with 
        //                                            | BackgroundImageFileValue.Singleton _ ->
        //                                                let index = (pageNumber-1) % totalPageNumber
        //                                                pageBoxs.[index], (reader.GetPage(index+1).CopyAsFormXObject(doc.Writer) :> PdfXObject)

        //                                            | BackgroundImageFileValue.Multiple _ ->
        //                                                pageBoxs.[0], (reader.GetPage(1).CopyAsFormXObject(doc.Writer) :> PdfXObject)
        //                                    |}

        //                                backgroundInfos

        //                            | _ -> 
        //                                let imageData = ImageDataFactory.Create(imageFile.Path)
        //                                let xobject = PdfImageXObject(imageData)
        //                                {|
        //                                    Close = ignore 
        //                                    PageBoxAndXObject = 
        //                                        let width = 
        //                                            xobject.GetWidth() 
        //                                            |> float
        //                                            |> fun m -> m / float (imageData.GetDpiX())
        //                                            |> inch

        //                                        let height = 
        //                                            xobject.GetHeight() 
        //                                            |> float
        //                                            |> fun m -> m / float (imageData.GetDpiY())
        //                                            |> inch

        //                                        let pageBox = Rectangle.create 0 0 (width) (height)
        //                                        //let fsRect = FsRectangle.OfRectangle pageBox
        //                                        pageBox, xobject :> PdfXObject
        //                                |}
        //                        )

        //                    match image with 
        //                    | BackgroundImageFileValue.Singleton image -> createImage image
        //                    | BackgroundImageFileValue.Multiple images -> createImage (images.[i])

        //                let addImage() =    
        //                    pdfCanvas.AddPdfXObjectDSLEx(
        //                        backgroundPageBox = fst image.PageBoxAndXObject,
        //                        backgroundXObject  = snd image.PageBoxAndXObject,
        //                        readerPageBox = readerPageBox,
        //                        xEffect = xEffect,
        //                        yEffect = yEffect,
        //                        positionTweak = (
        //                            match backgroundPositionTweak with 
        //                            | None -> BackgroundPositionTweak.DefaultValue
        //                            | Some backgroundPositionTweak -> backgroundPositionTweak typedPageNumber
        //                        ),
        //                        extGSState = extGSState
        //                    )
        //                    |> ignore
                            

        //                match layerName with 
        //                | None ->
        //                    match choice with 
        //                    | BackgroundOrForeground.Background ->
        //                        addImage()

        //                        pdfCanvas
        //                        |> addShadowColor pageSize
        //                        |> fun m -> m.AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                        |> ignore

        //                    | BackgroundOrForeground.Foreground -> 
        //                        pdfCanvas
        //                            .AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                        |> ignore

        //                        addImage()

        //                | Some layerName ->
        //                    let bklayer = 
        //                        layerName.BackgroundLayer.CreateLayer(doc.Writer)

        //                    let addBackground (pdfCanvas: PdfCanvas) =
        //                        pdfCanvas
        //                            .BeginLayerUnion(bklayer)
        //                            |> fun m -> 
        //                                addImage()
        //                                m.EndLayerUnion(bklayer)

        //                    let readerLayer = layerName.CurrentLayer.CreateLayer(doc.Writer)

        //                    let addReader ifAddShadowColor (pdfCanvas: PdfCanvas) =
        //                        pdfCanvas
        //                            .BeginLayerUnion(readerLayer)
        //                            |> fun m ->
                                        
        //                                match ifAddShadowColor with 
        //                                | true ->
        //                                    addShadowColor pageSize pdfCanvas
        //                                    |> ignore
        //                                | false -> ()

        //                                m
        //                                    .AddXObjectAbs(readerXObject, 0.f, 0.f)
        //                                    .EndLayerUnion(readerLayer)


        //                    match choice with 
        //                    | BackgroundOrForeground.Background ->
        //                        pdfCanvas
        //                        |> addBackground 
        //                        |> addReader true
        //                        |> ignore

        //                    | BackgroundOrForeground.Foreground -> 
        //                        pdfCanvas
        //                        |> addReader false
        //                        |> addBackground 
        //                        |> ignore

        //            | false ->
        //                let page = readerPage.CopyTo(doc.Writer)
        //                doc.Writer.AddPage(page)
        //                |> ignore
        //            )
        //            |> ignore


        //        imageCache
        //        |> List.ofSeq
        //        |> List.iter(fun m -> 
        //            m.Value.Close()
        //        )

        //    )
        //    |> reuse 
        //        "AddBackgroundOrForeground"
        //        [
        //            "pageSelector" => pageSelector.ToString()
        //            "backgroundFile" => image.ToString()
        //            "layer" => choice.ToString()
        //            "xEffect" => xEffect.ToString()
        //            "yEffect" => yEffect.ToString()
        //        ]

        static member AddBackgroundImage (backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(backgroundFile, BackgroundOrForeground.Background, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?backgroundPositionTweak = backgroundPositionTweak, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)

        static member AddForegroundImage (foregroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState) =
            Reuses.AddBackgroundOrForegroundImage(foregroundFile, BackgroundOrForeground.Foreground, ?shadowColor = shadowColor, ?xEffect = xEffect, ?yEffect = yEffect, ?backgroundPositionTweak = backgroundPositionTweak, ?pageSelector = pageSelector, ?layerName = layerName, ?extGSState = extGSState)



    type PdfRunner with 
        static member AddBackgroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddBackgroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?backgroundPositionTweak = backgroundPositionTweak,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)

        static member AddForegroundImage(pdfFile, backgroundFile, ?shadowColor, ?xEffect, ?yEffect, ?backgroundPositionTweak, ?pageSelector, ?layerName, ?extGSState, ?backupPdfPath) =
            Reuses.AddForegroundImage(
                backgroundFile,
                ?shadowColor = shadowColor,
                ?xEffect = xEffect,
                ?yEffect = yEffect,
                ?backgroundPositionTweak = backgroundPositionTweak,
                ?pageSelector = pageSelector,
                ?layerName = layerName,
                ?extGSState = extGSState

            )    
            |> PdfRunner.Reuse(pdfFile, ?backupPdfPath = backupPdfPath)



    type MultipleSlimBackground with 
        static member Create(
            backgroundFiles: PdfFile list,
            choice,
            configuration,
            ?layerName,
            ?xEffect,
            ?yEffect,
            ?shadowColor,
            ?extGsState,
            ?backgroundPositionTweak,
            ?xobjectOnly
            ) =
            let cache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
            let backgroundFiles = 
                backgroundFiles
                |> List.map(fun bk ->
                    cache.GetOrAdd(bk.FullPath, valueFactory = fun _ ->
                        BackgroundFile.Create(bk)
                    )
                )

            new 
                MultipleSlimBackground(
                    backgroundFiles,
                    choice,
                    configuration,
                    ?layerName               = layerName,
                    ?xEffect                 = xEffect,
                    ?yEffect                 = yEffect,
                    ?shadowColor             = shadowColor,
                    ?extGsState              = extGsState,
                    ?backgroundPositionTweak = backgroundPositionTweak,
                    ?xobjectOnly             = xobjectOnly
                )