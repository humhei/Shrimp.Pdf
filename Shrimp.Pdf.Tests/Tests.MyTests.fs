module MyTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
open System.IO
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus
open Shrimp.Pdf.RegisterableFonts
open Shrimp.Pdf.RegisterableFonts.YaHei
open FontNames.Query
open Shrimp.Pdf.Parser
open Fake.IO
open iText.IO.Font.Constants
open iText.IO.Font
open iText.Kernel.Font
open iText.Kernel.Pdf

let runTest (path: string) f =
    let testsPath = Path.changeExtension ".tests.pdf" path
    let flow =
        Manipulate(fun flowModel document ->
            let ops = defaultArg flowModel.Configuration.PdfModifyOptions PdfModifyOptions.DefaultValue
            let r = document.Value.GetPages() |> List.mapi(f)
            (document.Value :> IFsPdfDocumentEditor).Resources.DeleteRemovableXObject(ops)
            r

        )
        |> Flow.Manipulate
       
    runWithBackup testsPath path flow 

let myTests =
  testList "My Tests" [
    testList "read tests" [
        testCase "read specific layer datas" <| fun _ -> 
            let document = new PdfDocument(PdfReader(@"datas/read specific layer datas.pdf"))
            let selector = RenderInfoSelector.All(fun _ -> true)
            let layerOptions = ReaderLayerOptions.InShpLayer ([(ShpLayer.Foreground "fr")])
            let parser = NonInitialClippingPathPdfDocumentContentParser(document, layerOptions)
            let r   = NonInitialClippingPathPdfDocumentContentParser.parseIM 1 selector parser

            let a = 1
            ()

        testCase "read specific layer datas2" <| fun _ -> 
            let document = new PdfDocument(PdfReader(@"datas/read specific shpLayer datas.pdf"))
            let selector = RenderInfoSelector.All(fun _ -> true)
            let layerOptions = ReaderLayerOptions.InShpLayer ([(ShpLayer.CuttingDie(CuttingDieShpLayerInfosEnum.CuttingDie))])
            let parser = NonInitialClippingPathPdfDocumentContentParser(document, readerLayerOptions = layerOptions)
            let r   = 
                [1..100]
                |> List.map(fun _ -> NonInitialClippingPathPdfDocumentContentParser.parseIM 1 selector parser)

            let a = 1
            ()

        testCase "read path clipping info" <| fun _ ->


            runTest @"datas/read path clipping info.pdf" (fun i page ->
                let parser = NonInitialClippingPathPdfDocumentContentParser(page.GetDocument())
                let infos =     
                    NonInitialClippingPathPdfDocumentContentParser.parse 
                        (i+1) 
                        (RenderInfoSelector.PathOrText(fun _ -> true))
                        parser
                    |> List.ofSeq

                let colors =
                    infos
                    |> List.collect(fun m ->
                        [
                           m.Value.GetFillColor()
                           m.Value.GetStrokeColor()
                        ]
                        |> List.map FsColor.OfItextColor
                    )
                    |> FsColors.distinct
                    |> List.ofSeq

                ()

            )
            |> ignore

        testCase "read text clipping info" <| fun _ ->


            runTest @"datas/read text clipping info.pdf" (fun i page ->
                let parser = NonInitialClippingPathPdfDocumentContentParser(page.GetDocument())
                let infos =     
                    NonInitialClippingPathPdfDocumentContentParser.parse 
                        (i+1) 
                        (RenderInfoSelector.PathOrText(fun _ -> true))
                        parser
                    |> List.ofSeq

                let colors =
                    infos
                    |> List.collect(fun m ->
                        [
                           m.Value.GetFillColor()
                           m.Value.GetStrokeColor()
                        ]
                        |> List.map FsColor.OfItextColor
                    )
                    |> FsColors.distinct
                    |> List.ofSeq

                ()

            )
            |> ignore


        
        ftestCase "read colors" <| fun _ ->
            runTest @"C:\Users\Administrator\Desktop\121.pdf" (fun i page ->
                

                let parser = NonInitialClippingPathPdfDocumentContentParser(page.GetDocument())
                let infos =     
                    NonInitialClippingPathPdfDocumentContentParser.parse 
                        (i+1) 
                        (RenderInfoSelector.Path(fun _ -> true))
                        parser
                    |> List.ofSeq

                let colors =
                    infos
                    |> List.collect(fun m ->
                        [
                           m.Value.GetFillColor()
                           m.Value.GetStrokeColor()
                        ]
                        |> List.map FsColor.OfItextColor
                    )
                    |> FsColors.distinct
                    |> List.ofSeq

                ()

            )
            |> ignore

    
    ] 


    testList "layer tests" [
        testCase "remove layer contents" <| fun _ ->
            let pdfModifyOptions =
                PdfModifyOptions2.Create(
                    DocumentParserCache.Create(),
                    layerOptions = 
                        ModifyLayerOptions.RemoveLayer (
                            StreamableFsLayers.CustomLayer ["Layer2"]
                        )
                )

            runTest @"datas/remove layer contents.pdf" (fun i page ->
                PdfPage.modifyIM (pdfModifyOptions) Map.empty page
            )
            |> ignore

        testCase "remove layer contents2" <| fun _ ->

            let pdfModifyOptions =
                PdfModifyOptions2.Create(
                    DocumentParserCache.Create(),
                    layerOptions = 
                        ModifyLayerOptions.RemoveLayer (
                            StreamableFsLayers.CustomLayer ["Layer2"]
                        )
                )

            runTest @"datas/remove layer contents2.pdf" (fun i page ->
                PdfPage.modifyIM (pdfModifyOptions) Map.empty page
            )
            |> ignore

        testCase "remove shp layer contents" <| fun _ ->
            let pdfModifyOptions =
                PdfModifyOptions2.Create(
                    DocumentParserCache.Create(),
                    layerOptions = 
                        ModifyLayerOptions.RemoveShpLayer [
                            ShpLayer.Foreground "fr"
                        ] 
                )

            runTest @"datas/remove shp layer contents.pdf" (fun i page ->
                PdfPage.modifyIM (pdfModifyOptions) Map.empty page
            )
            |> ignore

        testCase "manipulate in shplayer" <| fun _ ->

            let pdfModifyOptions =
                PdfModifyOptions2.Create(
                    DocumentParserCache.Create(),
                    XObjectReference.ByRef,
                    layerOptions = 
                        ModifyLayerOptions.InShpLayer (
                        [(ShpLayer.CuttingDie(CuttingDieShpLayerInfosEnum.CuttingDie))]
                    )
                )

            let cuttingDieColor = FsColor.Separation cuttingLineSeparation


            let selectorMappings = 
                [
                    "Replace Color" =>
                        ModifierUnion.Modifier(fun args ->
                            ModifierPdfCanvasActions.CreateActions_All
                                args.CurrentRenderInfo.TagIM
                                [PdfCanvas.setStrokeColor DeviceRgb.MAGENTA]
                        )
                ]
                |> List.mapFst (fun name -> {Name = name})
                |> List.mapSnd(fun modifier ->
                    RenderInfoSelector.Path(fun info -> 
                        let strokeColor = info.PathRenderInfo.GetStrokeColor() |> FsColor.OfItextColor
                        match strokeColor with 
                        | EqualTo(cuttingDieColor) -> true
                        | _ -> false

                    ) => modifier
                )
                |> Map.ofList

            runTest @"datas/manipulate in shplayer.pdf" (fun i page ->
                let r = 
                    [1]
                    |> List.map(fun _ ->
                        PdfPage.modifyIM (pdfModifyOptions) selectorMappings page
                    )
                r.Head
            )
            |> ignore

        testCase "manipulate in layer" <| fun _ ->

            let pdfModifyOptions =
                PdfModifyOptions2.Create(
                    DocumentParserCache.Create(),
                    layerOptions = 
                        ModifyLayerOptions.InLayer (
                            StreamableFsLayers.CustomLayer ["Layer2"]
                    )
                )


            let selectorMappings = 
                [
                    "Replace Color" =>
                        ModifierUnion.Modifier(fun args ->
                            ModifierPdfCanvasActions.CreateActions_All
                                args.CurrentRenderInfo.TagIM
                                [PdfCanvas.setFillColor DeviceCmyk.BLACK]
                        )
                ]
                |> List.mapFst (fun name -> {Name = name})
                |> List.mapSnd(fun modifier ->
                    RenderInfoSelector.All(fun _ -> true ) => modifier
                )
                |> Map.ofList

            runTest @"datas/manipulate in layer.pdf" (fun i page ->
                PdfPage.modifyIM (pdfModifyOptions) selectorMappings page
            )
            |> ignore
        
    ]

    testList "reuses tests" [
        testCase "copy ocgs" (fun _ ->
            let path = @"D:\Users\Jia\Documents\MyData\Docs\2017\健耐\EAC\EAC\07NB-115-01E3AA__на короба_外箱.raw.pdf"
            let backup = Path.changeExtension ".backup.pdf" path

            let flow = Reuse(fun flowModel doc ->
                let fsOCProps =
                    let document = doc.Reader
                    document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.OCProperties)
                    |> FsOCProperties.Create

                doc.Reader.GetPages()
                |> List.iter(fun page ->
                    let page = page.CopyTo(doc.Writer)
                    doc.Writer.AddPage(page)
                    |> ignore
                )

                doc.CopyOCGS()

            )

            flow
            |> Flow.Reuse
            |> runWithBackup backup path 

            pass()
        )
        
    ]


  ]