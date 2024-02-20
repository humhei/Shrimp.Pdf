// Learn more about F# at http://fsharp.org
namespace Shrimp.Pdf
open Shrimp.FSharp.Plus
open Shrimp.Pdf
open System.IO
open System.Collections.Generic

#nowarn "0104"
open Shrimp.Pdf.PdfNames
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
open Shrimp.Pdf.Parser
#nowarn "0104"

[<AutoOpen>]
module _SlimFlow_BackgroundOrForeground =

    [<RequireQualifiedAccess>]
    type BackgroundPositionTweak =    
        | OfPageBoxCase of offsetX: float * offsetY: float * scale: option<float * float>
        | SpecficRect of FsRectangle
    with 
        static member OfPageBox(?offsetX, ?offsetY, ?scale) =
            BackgroundPositionTweak.OfPageBoxCase(defaultArg offsetX 0., defaultArg offsetY 0., scale)

        static member DefaultValue = OfPageBoxCase(0, 0, None)

        member this.Offset(x, y) =
            match this with 
            | OfPageBoxCase (offsetX, offsetY, scale) ->
                BackgroundPositionTweak.OfPageBoxCase(offsetX + x, offsetY + y, scale)

            | SpecficRect rect ->
                { rect with 
                    X = rect.X + x
                    Y = rect.Y + y }
                |> BackgroundPositionTweak.SpecficRect

        member this.SetScale(scale) =
            match this with 
            | OfPageBoxCase (offsetX, offsetY, _) ->
                BackgroundPositionTweak.OfPageBoxCase(offsetX, offsetY, scale)

            | SpecficRect rect ->
                match scale with 
                | None -> BackgroundPositionTweak.SpecficRect rect
                | Some scale ->
                    AffineTransform.GetScaleInstance(scale).Transform(rect.AsRectangle)
                    |> FsRectangle.OfRectangle
                    |> BackgroundPositionTweak.SpecficRect



    type WriteInfosFunc = 
        (*level_plus*)int -> PdfDocumentWithCachedResources -> Rectangle -> OffsetablePdfCanvas -> RenewableInfo list -> unit
    
    /// DefaultValue: SlimBackgroundRefOptions.Infos
    type SlimBackgroundRefOptions = 
        | Infos         = 0
        | XObject       = 1
        | XObject_Simply = 2

    type ImageSlimBackground
        (imageFile: FsFullPath,
         choice: BackgroundOrForeground,
         ?layerName: BackgroundAddingLayerOptions,
         ?xEffect: XEffort,
         ?yEffect: YEffort,
         ?shadowColor: NullablePdfCanvasColor,
         ?extGsState: FsExtGState,
         ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak) =
        
        let fileName = Path.GetFileNameWithoutExtension imageFile.Path
        let imageData = ImageDataFactory.Create(imageFile.Path)
        let xobject = PdfImageXObject(imageData)

        let pageBox = 
            let width = 
                xobject.GetWidth() 
                |> float
                |> fun m -> m / float (imageData.GetDpiX())
                |> inch

            let height = 
                xobject.GetHeight() 
                |> float
                |> fun m -> m / float (imageData.GetDpiY())
                |> inch

            let pageBox = Rectangle.create 0 0 (width) (height)
            //let fsRect = FsRectangle.OfRectangle pageBox
            pageBox

        let shpLayerName =
            match layerName with 
            | Some layerName -> layerName.BackgroundLayer.Name
            | None -> Path.GetFileNameWithoutExtension imageFile.Path

        let shpLayerPdfName = PdfName.ShpLayerPixel
          
        member x.PageBox = pageBox

        member x.XObject = xobject

        member x.Choice = choice
    
        member x.XEffect = xEffect

        member x.FileName = fileName
    
        member x.YEffect = yEffect
    
        member x.LayerName = layerName
    
        member x.ImageFile = imageFile
    
        member x.ShadowColor = shadowColor
    
        member x.ExtGsState = extGsState
    
        member x.BackgroundPositionTweak = backgroundPositionTweak
        
        member x.ShpLayerName = shpLayerName

        member x.ShpLayerPdfName = shpLayerPdfName

        member x.GeneratePageBoxAndXObject(writerDocument, writePageBox: Rectangle) =
            pageBox, xobject :> PdfXObject

    type SlimBackground
        (backgroundFile: BackgroundFile,
         choice: BackgroundOrForeground,
         configuration: PdfConfiguration,
         ?layerName: BackgroundAddingLayerOptions,
         ?xEffect: XEffort,
         ?yEffect: YEffort,
         ?shadowColor: NullablePdfCanvasColor,
         ?extGsState: FsExtGState,
         ?backgroundPositionTweak: PageNumber -> BackgroundPositionTweak,
         ?refOptions: SlimBackgroundRefOptions) =
        
        let cache = System.Collections.Concurrent.ConcurrentDictionary()
    
        let shpLayerName =
            match layerName with 
            | Some layerName -> layerName.BackgroundLayer.Name
            | None -> backgroundFile.Name

        let mutable isDisposed = false

        let tmpFile = Path.GetTempFileNameEx() |> Path.changeExtension ".pdf"
        do File.Copy(backgroundFile.ClearedPdfFile.Path, tmpFile, true)
    
        let pdfDocument = 
            new PdfDocument(new PdfReader(tmpFile))
    
        let page = pdfDocument.GetPage(1)
        let pageBox = page.GetActualBox()
    
        let refOptions = defaultArg refOptions SlimBackgroundRefOptions.Infos

        let shpLayerPdfName =   
            match refOptions with 
            | SlimBackgroundRefOptions.Infos ->
                match choice with 
                | BackgroundOrForeground.Background -> PdfName.ShpLayerBK
                | BackgroundOrForeground.Foreground -> PdfName.ShpLayerForeground

            | SlimBackgroundRefOptions.XObject 
            | SlimBackgroundRefOptions.XObject_Simply ->
                match choice with 
                   | BackgroundOrForeground.Background -> PdfName.ShpLayerBk_XObjectOnly
                   | BackgroundOrForeground.Foreground -> PdfName.ShpLayerFr_XObjectOnly

                
        let mutable infos = []
        let readInfos() =
            match infos.IsEmpty, refOptions with 
            | false, SlimBackgroundRefOptions.XObject_Simply -> ()
            | _ ->

                let logInfo (text) = 
                    let logger: PageLogger =
                        { LoggerLevel = configuration.LoggerLevel 
                          LoggingPageCountInterval = 10 }
    
                    logger.Log(text, alwaysPrintingConsole_If_Info = true)
    
                let stopWatch = System.Diagnostics.Stopwatch.StartNew()
                let parser = NonInitialClippingPathPdfDocumentContentParser(pdfDocument)
                let newInfos = 
                    NonInitialClippingPathPdfDocumentContentParser.parseIM
                        1
                        (RenderInfoSelector.All(fun _ -> true))
                        parser
    
                let newInfos = 
                    let infos = 
                        newInfos
                        |> List.ofSeq
                        |> List.map(fun m -> m.AsUnion.SetVisibleBound0())
                        |> List.filter(fun m -> m.LazyVisibleBound0.IsSome)
    
                    infos
                    |> List.map(fun info ->
                        info.Renewable()
                    )
                    |> List.map(fun m -> m.UpdateVisibleBound1())
    
                stopWatch.Stop()
                logInfo(fun () ->
                    sprintf "    [SlimBackground][%s] extracting slim background in %d/%d, found infos %d in %O" backgroundFile.Name 1 1 newInfos.Length stopWatch.Elapsed
                ) 1
    
                infos <- newInfos

        do
            match refOptions with 
            | SlimBackgroundRefOptions.XObject 
            | SlimBackgroundRefOptions.XObject_Simply -> ()
            | SlimBackgroundRefOptions.Infos -> readInfos()
                

        let modifyFlowNames = HashSet<string * list<string * string>>()
        let filterInfosCache = new System.Collections.Concurrent.ConcurrentDictionary<_, obj list>()


        member x.FileName = backgroundFile.Name

        member x.ModifyInfos(name, paramters, f) =
            match modifyFlowNames.Contains(name, paramters) with 
            | true -> ()
            | false ->
                let newInfos: RenewableInfo list = f infos
                infos <- newInfos
                filterInfosCache.Clear()
                modifyFlowNames.Add(name, paramters) |> ignore

        member x.CollectInfos<'T>(name: string, mapping: RenewableInfo list -> 'T list) =
            filterInfosCache.GetOrAdd(name, valueFactory = fun _ ->
                infos
                |> mapping
                |> List.map box
            )
            |> List.map (fun info -> info :?> 'T)

        member x.ChooseInfos<'T>(name: string, choose: RenewableInfo -> 'T option) =
            filterInfosCache.GetOrAdd(name, valueFactory = fun _ ->
                infos
                |> List.choose choose
                |> List.map box
            )
            |> List.map (fun info -> info :?> 'T)

        member x.FilterInfos<'T>(name: string, filter: RenewableInfo -> bool) =
            x.ChooseInfos<RenewableInfo>(name, fun info ->
                match filter info with 
                | true -> Some info
                | false -> None
            )


        member x.SetColor() =
            x.ModifyInfos("SetColor", [], fun infos ->
                infos
                |> List.map(fun m -> m.SetColor())
            )

        member x.Internal_RecoverVisibleBound_ForWrite() =
            match refOptions with 
            | SlimBackgroundRefOptions.XObject_Simply -> readInfos()
            | _ ->
                x.ModifyInfos("Internal_RecoverVisibleBound_ForWrite", [], fun infos ->
                    infos
                    |> List.map(fun m ->
                        m.Internal_RecoverVisibleBound_ForWrite().UpdateVisibleBound1()
                    )
                )

        member x.Choice = choice
    
        member x.Infos = infos
    
        member x.PageBox = pageBox
    
        member x.XEffect = xEffect
    
        member x.YEffect = yEffect
    
        member x.LayerName = layerName
    
        member x.BackgroundFile = backgroundFile
    
        member x.ShadowColor = shadowColor
    
        member x.ExtGsState = extGsState
    
        member x.BackgroundPositionTweak = backgroundPositionTweak
        
        member x.ShpLayerName = shpLayerName

        member x.ShpLayerPdfName = shpLayerPdfName

        member x.GeneratePageBoxAndXObject(writeInfosFunc: WriteInfosFunc, writerDocument, writePageBox: Rectangle) =
            let point = FsPoint.OfPoint writePageBox.LeftBottom 
            cache.GetOrAdd(point, valueFactory = fun _ ->
                match refOptions with 
                | SlimBackgroundRefOptions.Infos
                | SlimBackgroundRefOptions.XObject_Simply ->
                    let xobject = PdfFormXObject(pageBox) 
                    let canvas = OffsetablePdfCanvas(xobject, writerDocument)
                    writeInfosFunc 1 writerDocument writePageBox canvas infos
                    pageBox, xobject :> PdfXObject

                | SlimBackgroundRefOptions.XObject ->
                    let xobject = page.CopyAsFormXObject(writerDocument)
                    pageBox, xobject :> PdfXObject
            )
    
    
        interface System.IDisposable with 
            member x.Dispose() =
                match isDisposed with 
                | false ->
                    pdfDocument.Close()
                    cache.Clear()
                    File.delete tmpFile
                    isDisposed <- true

                | true -> ()
    
    [<RequireQualifiedAccess>]
    type ImagableSlimBackground =
        | Image of ImageSlimBackground
        | SlimBackground of SlimBackground
    with 
        member x.KeyPath = 
            match x with 
            | Image image -> image.ImageFile
            | SlimBackground bk -> bk.BackgroundFile.ClearedPdfFile.FullPath

        member x.FileName =
            match x with 
            | Image image -> image.FileName
            | SlimBackground bk -> bk.FileName

        member x.XEffect =
            match x with 
            | Image bk -> bk.XEffect
            | SlimBackground bk -> bk.XEffect

        member x.YEffect =
            match x with 
            | Image bk -> bk.YEffect
            | SlimBackground bk -> bk.YEffect

        member x.BackgroundPositionTweak =
            match x with 
            | Image bk -> bk.BackgroundPositionTweak
            | SlimBackground bk -> bk.BackgroundPositionTweak

        member x.ShadowColor =
            match x with 
            | Image bk -> bk.ShadowColor
            | SlimBackground bk -> bk.ShadowColor

        member x.ExtGsState =
            match x with 
            | Image bk -> bk.ExtGsState
            | SlimBackground bk -> bk.ExtGsState

        member x.ShpLayerName =
            match x with 
            | Image bk -> bk.ShpLayerName
            | SlimBackground bk -> bk.ShpLayerName

        member x.ShpLayerPdfName =
            match x with 
            | Image bk -> bk.ShpLayerPdfName
            | SlimBackground bk -> bk.ShpLayerPdfName

        member x.Choice =
            match x with 
            | Image bk -> bk.Choice
            | SlimBackground bk -> bk.Choice

        member x.LayerName =
            match x with 
            | Image bk -> bk.LayerName
            | SlimBackground bk -> bk.LayerName

        member x.PageBox =
            match x with 
            | Image bk -> bk.PageBox
            | SlimBackground bk -> bk.PageBox

        member x.Internal_RecoverVisibleBound_ForWrite() =
            match x with 
            | SlimBackground slimBackground -> slimBackground.Internal_RecoverVisibleBound_ForWrite()
            | Image _ -> ()

        member x.SetColor() =
            match x with 
            | SlimBackground slimBackground -> slimBackground.SetColor()
            | Image _ -> ()

        member x.ModifyInfos(name, parameters, f) =
            match x with 
            | SlimBackground bk -> bk.ModifyInfos(name, parameters, f)
            | Image _ -> ()

        member x.CollectInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.CollectInfos(name, f)
            | Image _ -> []

        member x.FilterInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.FilterInfos(name, f)
            | Image _ -> []

        member x.ChooseInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.ChooseInfos(name, f)
            | Image _ -> []

        member x.GeneratePageBoxAndXObject(writeInfosFunc: WriteInfosFunc, writerDocument, writePageBox: Rectangle) =
            match x with 
            | SlimBackground bk -> bk.GeneratePageBoxAndXObject(writeInfosFunc, writerDocument, writePageBox)
            | Image bk -> bk.GeneratePageBoxAndXObject(writerDocument, writePageBox)

        interface System.IDisposable with 
            member x.Dispose() =
                match x with 
                | SlimBackground slimBackground -> (slimBackground :> System.IDisposable).Dispose()
                | Image _ -> ()
                
    [<RequireQualifiedAccess>]
    type SlimBackgroundFileOrImageFile =
        | ImageFile of FsFullPath
        | BackgroundFile of BackgroundFile
    with 

        member x.Name =
            match x with 
            | ImageFile path -> Path.GetFileNameWithoutExtension path
            | BackgroundFile file -> file.Name

        member x.KeyPath =
            match x with 
            | ImageFile path -> path
            | BackgroundFile file -> file.ClearedPdfFile.FullPath

    type MultipleSlimBackground
        (backgroundFiles: SlimBackgroundFileOrImageFile list,
         choice: BackgroundOrForeground,
         configuration: PdfConfiguration,
         ?layerName: BackgroundAddingLayerOptions,
         ?xEffect,
         ?yEffect,
         ?shadowColor,
         ?extGsState,
         ?backgroundPositionTweak,
         ?refOptions) =
    
        let cache = new System.Collections.Concurrent.ConcurrentDictionary<_, _>()
        let slimBackgrounds =
            backgroundFiles
            |> List.map(fun bk ->
                
                cache.GetOrAdd(bk.KeyPath, valueFactory = fun _ ->
                    match bk with 
                    | SlimBackgroundFileOrImageFile.BackgroundFile bk ->
                        new SlimBackground(
                            bk,
                            choice,
                            configuration,
                            ?layerName = layerName,
                            ?xEffect = xEffect,
                            ?yEffect = yEffect,
                            ?shadowColor = shadowColor,
                            ?extGsState = extGsState,
                            ?backgroundPositionTweak = backgroundPositionTweak,
                            ?refOptions = refOptions)
                        |> ImagableSlimBackground.SlimBackground

                    | SlimBackgroundFileOrImageFile.ImageFile image ->
                        new ImageSlimBackground(
                            image,
                            choice,
                            ?layerName = layerName,
                            ?xEffect = xEffect,
                            ?yEffect = yEffect,
                            ?shadowColor = shadowColor,
                            ?extGsState = extGsState,
                            ?backgroundPositionTweak = backgroundPositionTweak)
                        |> ImagableSlimBackground.Image
                )
            )
    
        member x.AsList = slimBackgrounds
    
        member x.SlimBackgrounds = slimBackgrounds
    
        member x.Choice = choice


    
        member x.ModifyInfos(name, paramters, f) =
            for pair in cache do 
                pair.Value.ModifyInfos(name, paramters, f)

        member x.SetColor() =
            for pair in cache do 
                pair.Value.SetColor()

        member x.Configuration = configuration
    
        member x.LayerName = layerName
    
        member x.XEffect = xEffect
    
        member x.YEffect = yEffect
    
        member x.BackgroundFiles = backgroundFiles
    
        interface System.IDisposable with 
            member x.Dispose() =
                for pair in cache do
                    (pair.Value :> System.IDisposable).Dispose()                
                cache.Clear()
    
    type SolidSlimBackground(?args: PdfCanvasAddRectangleArguments, ?pageBoxKind: PageBoxKind, ?margin: Margin) =
        let pageBoxKind = defaultArg pageBoxKind PageBoxKind.ActualBox
        let args = defaultArg args PdfCanvasAddRectangleArguments.DefaultValue

        member x.Margin = margin

        member x.PageBoxKind = pageBoxKind

        member x.RectOptions = args

 

    [<RequireQualifiedAccess>]
    type SolidableSlimBackground =
        | SlimBackground of ImagableSlimBackground
        | Solid          of SolidSlimBackground
    with 
        member x.Internal_RecoverVisibleBound_ForWrite() =
            match x with 
            | SlimBackground slimBackground -> slimBackground.Internal_RecoverVisibleBound_ForWrite()
            | Solid _ -> ()

        member x.SetColor() =
            match x with 
            | SlimBackground slimBackground -> slimBackground.SetColor()
            | Solid _ -> ()

        member x.AsSlimBackground =
            match x with 
            | SlimBackground slimBackground -> Some slimBackground
            | Solid _ -> None 

        member x.ModifyInfos(name, parameters, f) =
            match x with 
            | SlimBackground bk -> bk.ModifyInfos(name, parameters, f)
            | Solid _ -> ()

        member x.CollectInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.CollectInfos(name, f)
            | Solid _ -> []

        member x.FilterInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.FilterInfos(name, f)
            | Solid _ -> []

        member x.ChooseInfos(name, f) =
            match x with 
            | SlimBackground bk -> bk.ChooseInfos(name, f)
            | Solid _ -> []

        interface System.IDisposable with 
            member x.Dispose() =
                match x with 
                | SlimBackground slimBackground -> (slimBackground :> System.IDisposable).Dispose()
                | Solid _ -> ()

    [<RequireQualifiedAccess>]
    type SlimBackgroundUnion =
        | Multiple  of MultipleSlimBackground
        | Singleton of ImagableSlimBackground
    with 
        member x.FileNames() =
            match x with 
            | Multiple v -> v.SlimBackgrounds |> List.map(fun m -> m.FileName)
            | Singleton v -> [v.FileName]

        member x.GetByPageNumber(pageNumber: PageNumber) =
            match x with 
            | Multiple v -> v.AsList.[pageNumber.Value-1]
            | Singleton v -> v
    

        member x.SetColor() =
            match x with 
            | Singleton v -> v.SetColor()
            | Multiple v  -> v.SetColor()

        member x.ModifyInfos(name, paramters, f) =
            match x with 
            | Singleton v -> v.ModifyInfos(name, paramters, f)
            | Multiple v  -> v.ModifyInfos(name, paramters, f)

        member x.FilterInfos(pageNumber, name, filter) =
            x.GetByPageNumber(pageNumber).FilterInfos(name, filter)

        interface System.IDisposable with 
            member x.Dispose() =    
                match x with 
                | Singleton v -> (v :> System.IDisposable).Dispose()                
                | Multiple v  -> (v :> System.IDisposable).Dispose() 
                