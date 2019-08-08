module Tests.Pdf
open Expecto
open System
open Atrous.Pdf
open Fake.IO.FileSystemOperators
open Types
open System.IO
open iText.Kernel.Font
open Atrous.Pdf.Reuses
open Atrous.Pdf.Manipulates
open Atrous.Pdf.Operators
open iText.Kernel.Colors
open Atrous.Pdf.Extensions
open Atrous.Utils
open Atrous.Pdf.Targets
open Fake.IO.Globbing.Operators
open Atrous.Pdf.Targets
open Atrous.Pdf.Colors
open iText.Kernel.Pdf.Colorspace
open iText.Kernel.Pdf
open Atrous.Pdf.Targets.Actions
open Manipulates
open Fake.IO
open Atrous.Pdf.Select
open Atrous.Pdf.Targets.Types
open Atrous.Entities.Types
open Atrous.Pdf.Colors.ColorConverter
open Atrous.Pdf.Filters
open Atrous.Extensions
open Atrous

let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
PdfFontFactory.RegisterDirectory globalConfig.GlobalDirs.FontDir |> ignore

let useBackFile testDir fileName f =
    let path = testDir</>fileName
    File.Copy(testDir</>"back"</>fileName,path,true)
    f path
    

let PdfTests =
  testList "ManipulateTests" [
    testCase "change stroke color" <| fun _ -> 
        useBackFile testDir "IRUN.pdf" (fun path ->
            [
                manipulates
                    [
                        //blackOrWhite
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "trim to b255" <| fun _ -> 
        useBackFile testDir "b255.pdf" (fun path ->
            [
                manipulate
                    (trimToStrokeColor DeviceRgb.BLUE PageBoxKind.AllBox)
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "rotate" <| fun _ -> 
        let path = testDir </> "rotate.pdf"
        [
            rotatePage 90 |> manipulate
        ]
        |> simplePreflight path
        
        pass()
    testCase "remove path whose stroke color is b255" <| fun _ -> 
        useBackFile testDir "removePath.pdf" (fun pdf ->
            [
                //removePathWithStrokeColor DeviceRgb.BLUE |> manipulate
            ]
            |> simplePreflight pdf
            
            pass()
        )
    testCase "remove path whose stroke color is CYAN" <| fun _ -> 
        useBackFile testDir "removePath2.pdf" (fun pdf ->
            [
                manipulates 
                    [
                        forPageNums (fun i -> i = 1) (removePathWithStrokeColors [DeviceCmyk.CYAN])
                    ]
            ]
            |> simplePreflight pdf
            
            pass()
        )
    testCase "remove dash path whose stroke color is CYAN" <| fun _ -> 
        useBackFile testDir "removePath3.pdf" (fun pdf ->
            [
                manipulates 
                    [
                        removePathWith [Path.dash]
                    ]
            ]
            |> simplePreflight pdf
            
            pass()
        )

    testCase "change dash pattern of path" <| fun _ -> 
        useBackFile testDir "changeDashPattern.pdf" (fun pdf ->
            [
                manipulates 
                    [
                        changeDashPatternOfPath [toInche 1.2]
                    ]
            ]
            |> simplePreflight pdf
            
            pass()
        )

    testCase "remove cutting line" <| fun _ -> 
        useBackFile testDir "removePath3.pdf" (fun pdf ->
            [
                manipulates 
                    [
                        Manipulates.removeImpressLine
                    ]
            ]
            |> simplePreflight pdf
            
            pass()
        )

    testCase "retain path whose stroke color is b255" <| fun _ -> 
        useBackFile testDir "retainPath.pdf" (fun pdf ->
            [
                manipulate
                    //(replaceStrokeColorOfPath DeviceCmyk.CYAN DeviceCmyk.MAGENTA)
                    (retainPathWithStrokeColors [DeviceRgb.BLUE])
            ]
            |> simplePreflight pdf
            
            pass()
        )
    testCase "retain path whose stroke color is cyan" <| fun _ -> 
        useBackFile testDir "retainPathCyan.pdf" (fun pdf ->
            [
                manipulate
                    //(replaceStrokeColorOfPath DeviceCmyk.CYAN DeviceCmyk.MAGENTA)
                    //Manipulates.retainImpressLineAI
                    (retainPathWithStrokeColors [DeviceCmyk.CYAN;Color._registion])
            ]
            |> simplePreflight pdf
            
            pass()
        )

    testCase "retain impress line whose stroke color is cyan" <| fun _ -> 
        useBackFile testDir "retainPathCyan.pdf" (fun pdf ->
            [
                manipulate
                    //(replaceStrokeColorOfPath DeviceCmyk.CYAN DeviceCmyk.MAGENTA)
                    Manipulates.retainImpressLineAI
            ]
            |> simplePreflight pdf
            
            pass()
        )

    testCase "normalize dash pattern" <| fun _ -> 
        useBackFile testDir "retainPathCyan.pdf" (fun pdf ->
            [
                manipulate
                    //(replaceStrokeColorOfPath DeviceCmyk.CYAN DeviceCmyk.MAGENTA)
                    Manipulates.retainImpressLineAI
            ]
            |> simplePreflight pdf
            
            pass()
        )


    testCase "replace b255 to megenta" <| fun _ -> 
        useBackFile testDir "replaceStokeColor.pdf" (fun path ->
            [ 
                manipulates 
                    [
                        //trimToStrokeBlue
                        (replaceStrokeColorOfPath [DeviceRgb.BLUE] DeviceCmyk.CYAN)
                        //extractBySelectors tables variable.MakesureSelectors
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "replace black to cyan" <| fun _ -> 
        useBackFile testDir "replaceColor.pdf" (fun path ->
            [ 
                manipulates 
                    [
                        //trimToStrokeBlue
                        replaceColor [DeviceGray.BLACK] DeviceCmyk.CYAN
                        //replaceColor DeviceRgb.RED DeviceCmyk.CYAN
                        //extractBySelectors tables variable.MakesureSelectors
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "replace cutting lines to cyan" <| fun _ -> 
        useBackFile testDir "replaceStrokeColor2.pdf" (fun path ->
            [ 
                manipulates 
                    [
                        //trimToStrokeBlue
                        (replaceStrokeColorOfPath Colors.cuttingLine DeviceCmyk.CYAN)
                        //replaceColor DeviceRgb.RED DeviceCmyk.CYAN
                        //extractBySelectors tables variable.MakesureSelectors
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "add text" <| fun _ -> 
        useBackFile testDir "addText.pdf" (fun path ->
            [
                manipulates
                    [
                        addText { PageBoxKind = PageBoxKind.CropBox; Orientation = Orientation.LeftBottom (0.,0.) } (TextArgument.simpleText "Hello")
                        addText { PageBoxKind = PageBoxKind.CropBox; Orientation = Orientation.LeftTop (0.,0.) } (TextArgument.simpleText "Go")
                        addText { PageBoxKind = PageBoxKind.CropBox; Orientation = Orientation.Bottom (0.,0.) } (TextArgument.simpleText "Go")
                        addTextToPageWithScale PageBoxKind.CropBox 1. ("Go") FontGen.heiti DeviceGray.BLACK
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "add line" <| fun _ -> 
        useBackFile testDir "addLine.pdf" (fun path ->
            [
                manipulates [addSeamMark]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "add cmyk" <| fun _ -> 
        useBackFile testDir "addLine.pdf" (fun path ->
            [
                manipulates
                    [addCMYKMarker]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "add seam text" <| fun _ -> 
        useBackFile testDir "addLine.pdf" (fun path ->
            [
                manipulates
                    [addSeamText "第一版"]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "add seam info" <| fun _ -> 
        let pos = Position.createSimple (Orientation.Bottom (toInche 15,toInche 2))
        useBackFile testDir "addLine.pdf" (fun path ->
            [
                manipulates
                    (
                        [
                            addSeamText "第一版"
                            addCMYKMarker
                            addSeamMark
                        ] 
                    )
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "remove registion color" <| fun _ -> 
        useBackFile testDir "registion.pdf" (fun path ->
            [
                manipulates
                    [
                        removePathWithStrokeColors [Color._registion]
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "retain registion color" <| fun _ -> 
        useBackFile testDir "retainRegistion.pdf" (fun path ->
            [
                manipulates
                    [
                        retainPathWithStrokeColors [Color._registion]
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "remove text with red fill" <| fun _ -> 
        useBackFile testDir "removeText.pdf" (fun path ->
            [
                manipulates
                    [
                        removeTextWithFillColors Colors.Btw.size
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )
    testCase "keep text with red fill" <| fun _ -> 
        useBackFile testDir "retainText.pdf" (fun path ->
            [
                manipulates
                    [
                        //retainTextWithFillColors [DeviceRgb.RED]
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "addBoundToText" <| fun _ -> 
        useBackFile testDir "AddBoundToText.pdf" (fun path ->
            [
                manipulates
                    [
                        addBorderToText DeviceCmyk.MAGENTA
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "addBoundToTextSimple" <| fun _ -> 
        useBackFile testDir "AddBoundToTextSimple.pdf" (fun path ->
            [
                manipulates
                    [
                        addBorderToText DeviceCmyk.MAGENTA
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "rename pantone Colors" <| fun _ -> 
        //let files = !!(root + "\Colors\Pantone+ Solid Coated\*.elc")
        //let output = root </> "Colors" </> "Pantone+ Solid Coated.fsx"
        let files = !!(root + "\Colors\TPX\*.elc")
        let output = root </> "Colors" </> "TPX.fsx"
        let strings = 
            files |> Seq.map (fun path ->
                async {
                    let dest = path |> Path.changeExtension ".pdf"
                    File.Move(path,dest)
                    return ()
                }
            ) 
            |> List.ofSeq
            |> Async.Parallel
            |> Async.RunSynchronously
        pass()


    testCase "read pantone Colors" <| fun _ -> 
        let files = !!(root + "\Colors\Pantone+ Solid Coated\*.elc")
        let output = root </> "Colors" </> "Pantone+ Solid Coated.fsx"
        //let files = !!(root + "\Colors\TPX\*.elc")
        //let output = root </> "Colors" </> "TPX.fsx"
        let strings = 
            files |> Seq.map (fun path ->
                async {
                    let value = 
                        [ 
                            Read.colors Bleed.None (fun colors _ -> 
                                colors 
                                |> List.ofSeq
                                |> List.choose (fun color ->
                                    match color with 
                                    | :? Separation as color -> 
                                        assert (color.GetColorValue() = [|1.f|])
                                        match color.GetColorSpace() with 
                                        | :? PdfSpecialCs.Separation as colorSpace ->
                                            match colorSpace.GetBaseCs() with 
                                            | :? PdfCieBasedCs.Lab ->
                                                let array = colorSpace.GetPdfObject() :?> PdfArray
                                                let colorName = array.GetAsName(1) |> ColorBook.normalizePdfName
                                                let colorValues = 
                                                    let stream = array.GetAsStream(3)
                                                    stream.GetAsArray(PdfName.Decode) |> Seq.map (fun pdfObject ->
                                                        let number = pdfObject :?> PdfNumber
                                                        number.DoubleValue() |> roundToInt
                                                    ) 
                                                    |> List.ofSeq
                                                    |> List.splitList
                                                    |> snd

                                                let colorHexLiteral = colorValues |> LabColor.encodeToString 
                                                sprintf "    | ``%s`` = %s" colorName colorHexLiteral 
                                                |> Some
                                            | _ -> Logger.notImplemented()
                                        | _ -> Logger.notImplemented()
                                    | _ -> Logger.notImplemented()
                                )
                            ) 
                        ]
                        |> runWithUserStateReturn path
                    return value
                }
            ) 
            |> List.ofSeq
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.concat
        File.WriteAllLines(output,strings)
        pass()
  ]

let RegionTests =
  let testDir = testDir </> "Region"
  testList "ManipulateTests" [
    testCase "replace b255 to megenta" <| fun _ -> 
        useBackFile testDir "replaceStokeColor.pdf" (fun path ->
            [ 
                manipulates 
                    [
            //trimToStrokeBlue
                        inRegion PageBoxKind.CropBox <| replaceStrokeColorOfPath [DeviceRgb.BLUE] DeviceCmyk.CYAN
                        //extractBySelectors tables variable.MakesureSelectors
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "read desired color cross page" <| fun _ -> 
        useBackFile testDir "replaceStokeColor.pdf" (fun path ->
            let colors =
                [ 
                    Read.colors Bleed.None (fun colors _ -> colors |> List.ofSeq) 
                ]
                |> runWithUserStateReturn path
            
            pass()
        )
    testCase "read desired color cliping" <| fun _ -> 
        useBackFile testDir "readDesiredColorCliping.pdf" (fun path ->
            [ 
                manipulates 
                    [
            //trimToStrokeBlue
                        inRegion PageBoxKind.CropBox <| replaceStrokeColorOfPath [DeviceRgb.BLUE] DeviceCmyk.CYAN
                        //extractBySelectors tables variable.MakesureSelectors
                    ]
            ]
            |> simplePreflight path
            
            pass()
        )

  ]


let ReuseTests =
    testList "Reuse Tests" [

    ftestCase "splitDocBy" <| fun _ ->
        useBackFile testDir "splitFile.pdf" (fun path ->
            [
                Read.subRects [AnalyticSubpath.sizeLitterBigger 60.<mm> 36.<mm>] (fun a _ -> a)
                Flow.TransformUserState (fun s ->
                    printf ""
                    s
                )

            ] 
            |> runWithUserStateReturn path
            |> ignore

            ()
        )


    testCase "init" <| fun _ -> 
        let path = testDir </> "temp.pdf"
        createPdf path (List.replicate 10 FsSize.A4)
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
        pass()

    testCase "impose" <| fun _ -> 
        let path = testDir </> "impose.pdf"
        [clockwise()]
        |> simplePreflight path
        
        pass()

    testCase "rotatePage" <| fun _ -> 
        let path = testDir </> "rotate.pdf"
        clockwise()
        |> List.singleton
        |> simplePreflight path
        
        pass()

    testCase "rotate2" <| fun _ -> 
        let path = testDir </> "b255.pdf"
        impose
            {
                ColNums = [1]
                RowNum = 1
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.createSimple 0.
                UseBleed = false
                Background = Background.None
                RotateXObjectDegree = Rotation.Clockwise
                DesiredSize = None
                IsRepeated = false
                //DesiredSize = Some {Width = mm 25.; Height = mm 30.}
            }
        |> List.singleton
        |> simplePreflight path
        
        pass()

    testCase "desiredSize" <| fun _ -> 
        let path = testDir </> "b255.pdf"
        impose
            {
                ColNums = [2]
                RowNum = 2
                Cropmark = None
                HSpaces = [3.]
                VSpaces = [3.]
                Margin = Margin.createSimple 10.
                UseBleed = true
                Background = Background.None
                RotateXObjectDegree = Rotation.Clockwise
                DesiredSize = Some {Width = toInche 25.; Height = toInche 30.}
                IsRepeated = false
                //RotateXObjectDegree = None
                //DesiredSize = None
            }
        |> List.singleton
        |> simplePreflight path
        
        pass()
    testCase "desiredSize2" <| fun _ -> 
        useBackFile testDir  "resize.pdf" <| fun path ->
            resize {Width = toInche 30.; Height = toInche 30.}
            |> List.singleton
            |> simplePreflight path
            
            pass()
    testCase "fix rotation" <| fun _ -> 
        useBackFile testDir "FixRotation.pdf" (fun path ->
            [
                impose
                    { ImposingArgument.dummy with 
                        ColNums = [1]
                        RowNum = 1 }
            ]
            |> simplePreflight path
            
            pass()
        )

    testCase "merge" <| fun _ -> 
        let path = testDir </> "Merge.pdf"
        let ext = testDir </> "External.pdf"
        merge ext 
        |> List.singleton
        |> simplePreflight path

    testCase "oneColumn" <| fun _ -> 

        useBackFile testDir "OneColumn.pdf" <| fun path ->
            oneColumn() 
            |> List.singleton
            |> simplePreflight path
  ]
