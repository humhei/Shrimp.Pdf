module MyTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
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
            let page = document.Value.GetPage(1)
            let r = f page
            (document.Value :> IFsPdfDocumentEditor).Resources.DeleteRemovableXObject(ops)
            r

        )
        |> Flow.Manipulate
       
    runWithBackup testsPath path flow 

let myTests =
  testList "My Tests" [
    testCase "read specific layer datas" <| fun _ -> 
        let document = new PdfDocument(PdfReader(@"datas/read specific layer datas.pdf"))
        let selector = RenderInfoSelector.All(fun _ -> true)
        let layerOptions = ReaderLayerOptions.SpecificLayers [FsLayer.ShpLayer (ShpLayer.Foreground "fr")]
        let parser = NonInitialClippingPathPdfDocumentContentParser(document)
        let r   = 
            NonInitialClippingPathPdfDocumentContentParser.parseIM 1 selector parser

        let a = 1
        ()

    testCase "remove layer contents" <| fun _ ->
        let pdfModifyOptions =
            PdfModifyOptions2.Create(
                DocumentParserCache.Create(),
                layerOptions = 
                    ModifyLayerOptions.RemoveLayer [
                        FsLayer.CustomLayer "Layer2"
                    ]
            )

        runTest @"datas/remove layer contents.pdf" (fun page ->
            PdfPage.modifyIM (pdfModifyOptions) Map.empty page
        )
        |> ignore

    ftestCase "remove layer contents2" <| fun _ ->

        let pdfModifyOptions =
            PdfModifyOptions2.Create(
                DocumentParserCache.Create(),
                layerOptions = 
                    ModifyLayerOptions.RemoveLayer [
                        FsLayer.CustomLayer "Layer2"
                    ]
            )

        runTest @"datas/remove layer contents2.pdf" (fun page ->
            PdfPage.modifyIM (pdfModifyOptions) Map.empty page
        )
        |> ignore

    testCase "remove shp layer contents" <| fun _ ->
        let pdfModifyOptions =
            PdfModifyOptions2.Create(
                DocumentParserCache.Create(),
                layerOptions = 
                    ModifyLayerOptions.RemoveLayer [
                        FsLayer.ShpLayer (ShpLayer.Foreground "fr")
                    ]
            )

        runTest @"datas/remove shp layer contents.pdf" (fun page ->
            PdfPage.modifyIM (pdfModifyOptions) Map.empty page
        )
        |> ignore

    testCase "manipulate in layer2" <| fun _ ->

        let pdfModifyOptions =
            PdfModifyOptions2.Create(
                DocumentParserCache.Create(),
                layerOptions = 
                    ModifyLayerOptions.InLayer [
                        FsLayer.CustomLayer "Layer2"
                    ]
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

        runTest @"datas/manipulate in layer2.pdf" (fun page ->
            PdfPage.modifyIM (pdfModifyOptions) selectorMappings page
        )
        |> ignore


  ]