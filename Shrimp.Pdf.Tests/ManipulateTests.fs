module ManipulateTests
open Expecto
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open iText.Layout
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf.Extract
open Shrimp.Pdf.icms2
open Shrimp.FSharp.Plus
open Shrimp.Pdf.RegisterableFonts
open Shrimp.Pdf.RegisterableFonts.YaHei
open FontNames.Query
open Imposing
open Shrimp.Pdf.Parser
open Fake.IO
open iText.IO.Font.Constants
open iText.IO.Font
open iText.Kernel.Font
open Shrimp.Pdf.Image

[<RequireQualifiedAccess>]
module PageInfos =
    open FParsec
    let sizeParser() = 
        let pMultiple () = anyOf [ 'x'; '×'; '*' ]

        let parser =
            let unitParser =
                (pstring "mm" >>% 1) <|> (pstring "cm" >>% 10)

            (pfloat .>> (pMultiple () <|> (unitParser >>. pMultiple()))
             .>>. pfloat
             .>>. unitParser
             |>> (fun ((width, height), unit) ->
                 { Width = (width * float unit)
                   Height = (height * float unit) }))

        parser


let manipulateTests =
  testList "Manipulates Tests" [
    testCase "remove layer contents" <| fun _ ->  

        Flow.Manipulate (
            ModifyPage.RemoveLayer("Layer2")
        )
        |> runTest "datas/manipulate/remove layer contents.pdf" 
        |> ignore

    testCase "remove layer contents2" <| fun _ ->  

        Flow.Manipulate (
            ModifyPage.RemoveLayer("Layer2")
        )
        |> runTest "datas/manipulate/remove layer contents2.pdf" 
        |> ignore

    testCase "remove ICC" <| fun _ ->  

        Flow.Manipulate (
            Modify.RemoveICC()
        )
        |> runTest "datas/manipulate/removeICC.pdf" 
        |> ignore


    testCase "read shading colors" <| fun _ ->  
        let path = "datas/manipulate/read shading colors.pdf" 
        PdfRunner.ReadColors(PdfFile path)
        |> ignore

    testCase "read shading colors2" <| fun _ ->  
        let path = "datas/manipulate/read shading colors2.pdf" 
        let colors = PdfRunner.ReadColors(PdfFile path)
        ()

    testCase "read shading colors3" <| fun _ ->  
        let path = "datas/manipulate/read shading colors3.pdf" 
        let colors = PdfRunner.ReadColors(PdfFile path)
        ()


    testCase "read shading colors4" <| fun _ ->  
        let path = "datas/manipulate/read shading colors4.pdf" 
        let colors = PdfRunner.ReadColors(PdfFile path)
        ()
        
    ptestCase "read complex infos" <| fun _ ->  
        let path = "datas/manipulate/read complex infos.pdf" 
        let colors = PdfRunner.ReadColors(PdfFile path)
        ()

    testCase "resize path as copy" <| fun _ ->     
        let resizingStyle = 
            ResizingStyle
                { Width = mm 43.
                  Height = mm 73. }

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.BoundSizeIs(fun size -> size.GetWidthF() >= mm 30.))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Resize resizingStyle, asCopy = true)
            )
        )
        |> runTest "datas/manipulate/resize path as copy.pdf" 
        |> ignore

    testCase "flip path as copy" <| fun _ ->     
        let resizingStyle = 
            FlipStyle(Flip.HFlip)

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.StrokeColorIs (FsColor.valueColor FsDeviceCmyk.MAGENTA))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Flip resizingStyle, asCopy = false)
            )
        )
        |> runTest "datas/manipulate/flip path as copy.pdf" 
        |> ignore
       
    testCase "flip path as copy2" <| fun _ ->     
        let resizingStyle = 
            FlipStyle(Flip.VFlip, PageBoxKind.CropBox)

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.StrokeColorIs (FsColor.valueColor FsDeviceCmyk.MAGENTA))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Flip resizingStyle, asCopy = false)
            )
        )
        |> runTest "datas/manipulate/flip path as copy2.pdf" 
        |> ignore

    testCase "flip path as copy3" <| fun _ ->     
        let resizingStyle = 
            FlipStyle(Flip.VFlip)

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.StrokeColorIs (FsColor.valueColor FsDeviceCmyk.MAGENTA))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Flip resizingStyle, asCopy = false)
            )
        )
        |> runTest "datas/manipulate/flip path as copy3.pdf" 
        |> ignore

    testCase "flip path as copy4" <| fun _ ->     
        let resizingStyle = 
            FlipStyle(Flip.VFlip, PageBoxKind.ActualBox)

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.StrokeColorIs (FsColor.valueColor FsDeviceCmyk.MAGENTA))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Flip resizingStyle, asCopy = false)
            )
        )
        |> runTest "datas/manipulate/flip path as copy4.pdf" 
        |> ignore


    testCase "flip path as copy5" <| fun _ ->     
        let resizingStyle = 
            FlipStyle(Flip.HFlip, PageBoxKind.TrimBox)

        let color = FsSeparation.Create ("Braille", FsValueColor.CMYK_CYAN)

        Flow.Manipulate (
            Modify.ChangeStyle(
                selector = (
                    Selector.Path(Info.FillColorIs (FsColor.Separation color))
                ),
                targetStyle = VectorStyle(transformStyle = TransformStyle.Flip resizingStyle, asCopy = false)
            )
        )
        |> runTest "datas/manipulate/flip path as copy5.pdf" 
        |> ignore

    testCase "release compound path" <| fun _ -> 
        Flow.Manipulate (
            Modify.ReleaseCompoundPath(Info.StrokeColorIs FsColor.RGB_BLUE)
        )
        |> runTest "datas/manipulate/release compound path.pdf" 
        |> ignore

    testCase "release compound path2" <| fun _ -> 
        Flow.Manipulate (
            Modify.ReleaseCompoundPath(Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation))
        )
        |> runTest "datas/manipulate/release compound path2.pdf" 
        |> ignore

    testCase "release compound path3" <| fun _ -> 
        Flow.Manipulate (
            Modify.ReleaseCompoundPath(fun _ _ -> true)
        )
        |> runTest "datas/manipulate/release compound path3.pdf" 
        |> ignore

    testCase "make combound path from blue strokes" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateCompoundPath(Info.StrokeColorIs FsColor.RGB_BLUE)
            <+>
            Modify.OpenFill(Selector.Path(fun _ _ -> true))
        )
        |> runTest "datas/manipulate/make combound path from blue strokes.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateClippingPath(Info.StrokeColorIs FsColor.RGB_BLUE)
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes2" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateClippingPath(
                Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation),
                keepCompoundPath = true,
                condition = ClippingCondition.ClipIfPathCountSmallerOrEqualThan 2

            )
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes2.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes3" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateClippingPath(
                Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation),
                keepCompoundPath = true,
                condition = ClippingCondition.ClipIfPathCountSmallerOrEqualThan 2

            )
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes3.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes4" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateClippingPath(
                Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation),
                keepCompoundPath = true,
                condition = ClippingCondition.ClipIfPathCountSmallerOrEqualThan 5

            ) 
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes4.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes by minimum area" <| fun _ ->    
        let pdfPath = 
            @"D:\Users\Jia\Documents\MyData\Docs\2017\新纪元\Lolly Tree\.btw\新纪元 新纪元 22-12-13\吊牌反面.productImage\.shrimp.pdf\吊牌反面.down.productImage\0_SequencePages.pdf"
        

        Flow.Manipulate (
            Modify.CreateClippingPath(
                Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation),
                condition = ClippingCondition.ClipIfPathCountSmallerOrEqualThan 2
            )
        )
        |> runTest pdfPath
        //|> runTest "datas/manipulate/make clipping path from blue strokes by minimum area.pdf" 
        |> ignore

    testCase "make clipping path from blue strokes and keep" <| fun _ -> 
        Flow.Manipulate (
            Modify.CreateClippingPath(Info.StrokeColorIs (FsColor.Separation cuttingLineSeparation), keepCompoundPath = true)
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes and keep.pdf" 
        |> ignore


    testCase "make clipping path from blue strokes and keep2" <| fun _ -> 
        let cuttingLine = FsColor.Separation cuttingLineSeparation

        Flow.Manipulate (
            Modify.CreateClippingPath(Info.StrokeColorIs cuttingLine, keepCompoundPath = false)
        )
        |> runTest "datas/manipulate/make clipping path from blue strokes and keep2.pdf" 
        |> ignore

    testCase "clipping Contents To Pagebox" <| fun _ -> 

        Flow.Manipulate (
            //ModifyPage.Create(
            //    "AddRect",
            //    PageSelector.All,
            //    Dummy,
            //    PageModifier.AddRectangleToCanvasRootArea(AreaGettingOptions.PageBoxWithOffset (PageBoxKind.ActualBox, -Margin.MM3), fun args ->
            //        { args with FillColor = NullablePdfCanvasColor.valueColor FsDeviceCmyk.MAGENTA}
            //    )
            //)
            ModifyPage.ClippingContentsToPageBox(PageBoxKind.ActualBox, -Margin.MM3)
        )
        |> runTest "datas/manipulate/clipping Contents To Pagebox.pdf" 
        |> ignore


    testCase "expand stroke width" <| fun _ -> 
        Flow.Manipulate (
            Modify.ExpandStrokeWidth(
                [FsColor.CMYK_BLACK; FsColor.BLACK],
                mmZ 0.8,
                PdfCanvasColor.valueColor FsDeviceCmyk.MAGENTA)
        )
        |> runTest "datas/manipulate/expand black stroke.pdf" 
        |> ignore

    testCase "read separation colors" <| fun _ -> 
        let pdfFile = PdfFile @"datas/manipulate/read separation colors.pdf"
        let infos =
            PdfRunner.ReadColors(
                pdfFile
            )

        ()
       
    testCase "read separation colors2" <| fun _ -> 
        let pdfFile = PdfFile @"datas/manipulate/read separation colors2.pdf"
        let testFile = Path.changeExtension ".tests.pdf" (pdfFile.Path)
        let infos =
            PdfRunner.ReadColors(
                pdfFile
            )

        ()


    testCase "read separation colors3" <| fun _ -> 
        let pdfFile = PdfFile @"datas/manipulate/read separation colors3.pdf"
        let testFile = Path.changeExtension ".tests.pdf" (pdfFile.Path)
        let infos =
            PdfRunner.ReadColors(
                pdfFile
            )

        ()
    
    testCase "modify tests" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    SelectorAndModifiers(
                        "modify tests",
                        Dummy,
                        []
                    )
                ]
            ) 
        )
        |> runTest "datas/manipulate/modify.pdf" 
        |> ignore

    testCase "change separation color of pdfFunction2 PageNumber to m100" <| fun _ -> 
        
        let pageNumberSeparationColor: PdfCanvasColor =
            FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            |> PdfCanvasColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    SelectorAndModifiers(
                        "change separation color of pdfFunction2 PageNumber to m100",
                        Text(Info.ColorIsOneOf (FillOrStrokeOptions.Fill, [PdfCanvasColor.Registration ;pageNumberSeparationColor])),
                        [ Modifier.SetFillColor(DeviceCmyk.MAGENTA) ]
                    )
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction2 PageNumber to m100.pdf" 
        |> ignore

    testCase "remove R255B255" <| fun _ -> 

        let colors = 
            [
                PdfCanvasColor.valueColor FsDeviceRgb.MAGENTA
            ]

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "remove specfic separation colors"
                      Selector = 
                        PathOrText(Info.ColorIsOneOf (FillOrStrokeOptions.FillOrStroke, colors))
                      Modifiers = [Modifier.CancelFillAndStroke()]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/remove R255B255.pdf" 
        |> ignore

    testCase "remove specfic separation colors" <| fun _ -> 

        let colors = 
            [
                { Name = "CuttingLine_BLUE" 
                  BaseColor = FsValueColor.RGB_BLUE
                  Transparency = 1. }

                FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            ]
            |> List.map PdfCanvasColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "remove specfic separation colors"
                      Selector = 
                        PathOrText(Info.ColorIsOneOf (FillOrStrokeOptions.FillOrStroke, colors))
                      Modifiers = [Modifier.CancelFillAndStroke()]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/remove specfic separation colors.pdf" 
        |> ignore

    testCase "remove specfic separation colors2" <| fun _ -> 

        let colors = 
            [
                //{ Name = "CuttingLine_BLUE" 
                //  BaseColor = FsValueColor.RGB_BLUE
                //  Transparency = 1. }
                FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            ]
            |> List.map PdfCanvasColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "remove specfic separation colors"
                      Selector = 
                        PathOrText(Info.ColorIsOneOf (FillOrStrokeOptions.FillOrStroke, colors))
                      Modifiers = [Modifier.CancelFillAndStroke()]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/remove specfic separation colors2.pdf" 
        |> ignore

    testCase "open fill color" <| fun _ -> 

        let colors = 
            [
                { Name = "CuttingLine_BLUE" 
                  BaseColor = FsValueColor.RGB_BLUE
                  Transparency = 1. }

                FsSeparation.Create("PageNumber", DeviceRgb(200, 0, 56))
            ]
            |> List.map FsColor.Separation

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "open fill color"
                      Selector = PathOrText(Info.StrokeColorIsOneOf (colors))
                      Modifiers = [Modifier.OpenFill(PdfCanvasColor.WHITE)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/open fill color.pdf" 
        |> ignore

    testCase "change separation color of pdfFunction0 PageNumber to m100" <| fun _ -> 

        let pantone100C: PdfCanvasColor =
            PdfCanvasColor.ColorCard (ColorCard.Pantone PantoneColorEnum.``PANTONE 100 C``)

        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change separation color of pdfFunction0 PageNumber to m100"
                      Selector = Path(Info.FillColorIs pantone100C)
                      Modifiers = [Modifier.SetFillColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction0 PageNumber to m100.pdf" 
        |> ignore

    testCase "change separation color of pdfFunction0 Registration to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change separation color of pdfFunction0 Registration to m100"
                      Selector = Path(Info.FillColorIs PdfCanvasColor.Registration)
                      Modifiers = [Modifier.SetFillColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change separation color of pdfFunction0 Registration to m100.pdf" 
        |> ignore

    testCase "change stroke color b255 to m100_0" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.Expr(PageSelectorExpr.create "1"),
                [
                    { Name = "change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs FsColor.RGB_BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change stroke color b255 to m100_0.pdf" 
        |> ignore

    testCase "change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.Expr(PageSelectorExpr.create "2-R1"),
                [
                    { Name = "change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs FsColor.RGB_BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "change stroke color b255 to m100_2" <| fun _ -> 
        Flow.Reuse(
            Reuses.DuplicatePages(PageSelector.All, CopiedNumSequence.Create [5])
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.Expr(PageSelectorExpr.create "2"),
                [
                    { Name = "change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs FsColor.RGB_BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change stroke color b255 to m100_2.pdf" 
        |> ignore

    testCase "change stroke color b255 to m100_3" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.Expr(PageSelectorExpr.create "1"),
                [
                    { Name = "change stroke color b255 to m100"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_BLUE)
                      Modifiers = [
                        ( fun args ->
                            Modifier.SetStrokeColor(DeviceCmyk.MAGENTA) args 
                        )
                    ]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change stroke color b255 to m100_3.pdf" 
        |> ignore

    testCase "change red to black" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change red to black"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_RED)
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change red to black.pdf" 
        |> ignore

    testCase "change red to black NUped" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change red to black NUped"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_RED)
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change red to black NUped.pdf" 
        |> ignore

    testCase "change red to black outside of trimbox" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "change red to black outside of trimbox"
                      Selector = PathOrText(Info.FillColorIs FsColor.RGB_RED <&&> Info.BoundIsOutsideOf(AreaGettingOptions.PageBox PageBoxKind.TrimBox))
                      Modifiers = [Modifier.SetFillColor(DeviceGray.BLACK)]
                    }
                ]
            ) 
        )
        |> runTest "datas/manipulate/change red to black outside of trimbox.pdf" 
        |> ignore

    testCase "change gold to black" <| fun _ ->
        let Gold: FsSeparation =
            FsSeparation.Create("Gold", DeviceRgb(239, 227, 131))

        Flow.Manipulate(
            Modify.ReplaceColors(
                picker = (fun color' ->
                    match color' with 
                    | FsColor.EqualTo (FsColor.Separation Gold) -> Some (FsValueColor.ToItextColor FsValueColor.BLACK)
                    | _ -> None
                )
            )
        )
        |> runTest "datas/manipulate/change gold to black.pdf" 
        |> ignore

    testCase "change light blue to separation" <| fun _ ->
        let lightBlue = 
            FsDeviceCmyk.OfLoggingText_Raw "CMYK 47.7 4.0 3.3 0.0"
            |> FsValueColor.Cmyk

        let lightBlue_Sepration: FsSeparation =
            FsSeparation.Create("样品天蓝色", lightBlue)

        Flow.Manipulate(
            Modify.ReplaceColors(
                colorMapping =
                    ColorMappings(AtLeastOneList.Create [
                        { OriginColors = 
                            AtLeastOneList.Create [
                                AlternativeFsColor.ValueColor lightBlue
                            ]

                          NewColor = NullablePdfCanvasColor.Separation lightBlue_Sepration
                          Tolerance = ValueEqualOptionsTolerance.Rough
                        
                        }
                    ])
            )
        )
        |> runTest "datas/manipulate/change light blue to separation.pdf" 
        |> ignore

    testCase "change rgb gray to gray" <| fun _ ->



        let rgb_gray =
            FsDeviceRgb.Create(220, 220, 220)

        let toGray (rgbColor: FsDeviceRgb)= 
            if rgbColor.R = rgbColor.B 
                && rgbColor.R = rgbColor.G
            then 
                rgbColor.R
                |> FsGray
                |> FsValueColor.Gray
                |> FsValueColor.ToItextColor
                |> Some
            else None

        let rgb_gray_fsColor =
            rgb_gray
            |> FsValueColor.Rgb
            |> FsColor.ValueColor


        Flow.Manipulate(
            Modify.ReplaceColors(
                picker = (fun color' ->
                    match color' with 
                    | FsColor.EqualTo rgb_gray_fsColor -> toGray rgb_gray
                    | _ -> None
                )
            )
        )
        |> runTest "datas/manipulate/change rgb gray to gray.pdf" 
        |> ignore

    testCase "xobject_change stroke color b255 to m100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs FsColor.RGB_BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)] }
                ]
            )
        )
        |> runTest "datas/manipulate/xobject_change stroke color b255 to m100.pdf" 
        |> ignore

    testCase "xobject_change stroke color b255 to m100 and then change m100 to c100" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color b255 to m100"
                      Selector = Path(Info.StrokeColorIs FsColor.RGB_BLUE)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.MAGENTA)] }
                ]
            )
            <+>
            Modify.Create (
                PageSelector.First,
                [
                    { Name = "xobject_change stroke color m100 to c100"
                      Selector = Path(Info.StrokeColorIs FsColor.CMYK_MAGENTA)
                      Modifiers = [Modifier.SetStrokeColor(DeviceCmyk.CYAN)] }
                ]
            )
        )
        |> runTest "datas/manipulate/xobject_change stroke color b255 to m100 and then change m100 to c100.pdf" 
        |> ignore



    testCase "add bound to text" <| fun _ -> 
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text.pdf" 
        |> ignore

    testCase "add bound to text3" <| fun _ -> 
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text3.pdf" 
        |> ignore

    testCase "add bound to text4" <| fun _ -> 
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor (PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to text4.pdf" 
        |> ignore

    testCase "add bound to text2" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text2.pdf" 
        |> ignore

    testCase "add bound to text6" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text6.pdf" 
        |> ignore

    testCase "add bound to text7" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text7.pdf" 
        |> ignore

    testCase "add bound to text8" <| fun _ -> 
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text8.pdf" 
        |> ignore

    testCase "add bound to images" <| fun _ -> 
        Flow.Manipulate (
            ModifyIM.AddBoundToImages()
        )
        |> runTest "datas/manipulate/add bound to images.pdf" 
        |> ignore

    testCase "decode text" <| fun _ -> 
        Flow.Manipulate (
            Modify.DecodeText() 
        )
        |> runTest "datas/manipulate/decode text.pdf" 
        |> ignore

    testCase "decode text2" <| fun _ -> 
        Flow.Manipulate (
            Modify.DecodeText() 
        )
        |> runTest "datas/manipulate/decode text2.pdf" 
        |> ignore

    testCase "add bound to text9" <| fun _ -> 
        Flow.Manipulate (
            Modify.AddBoundToTexts()
        )
        |> runTest "datas/manipulate/add bound to text9.pdf" 
        |> ignore

    testCase "add bound to text10" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Reuse (
            Reuses.ExtractIM(
                PageSelector.All,
                Selector.All(InfoIM.BoundIs_InsideOrCross_Of (AreaGettingOptions.PageBox PageBoxKind.ActualBox))
            )
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text8"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to text10.pdf" 
        |> ignore

    testCase "add bound to vertical text" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text5"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to vertical text.pdf" 
        |> ignore

    testCase "add bound to vertical text2" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to text5"
                      Selector = Text(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to vertical text2.pdf" 
        |> ignore

    testCase "add bound to bound1" <| fun _ -> 
        Flow.Reuse(
            Reuses.ClearDirtyInfos()
        )
        <+>
        Flow.Manipulate (
            Modify.Create(
                PageSelector.All,
                [
                    { Name = "add bound to bound"
                      Selector = Path(fun _ _ -> true) 
                      Modifiers = [
                        Modifier.AddRectangleToBound(fun args -> 
                            { args with StrokeColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA)}
                        )
                      ]
                    }
                ]
            )
        )
        |> runTest "datas/manipulate/add bound to bound1.pdf" 
        |> ignore


    testCase "add line to position" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add line to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.Batch [

                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.LeftMiddle (0., 0.),
                      Position.RightMiddle (0., 0.),
                      (fun args ->
                          { args with 
                                StrokeColor = PdfCanvasColor.Registration
                                DashPattern = DashPattern.Create(mm 5.)}
                      )
                    ) 

                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.TopMiddle (0., 0.),
                      Position.BottomMiddle (0., 0.),
                      (fun args ->
                          { args with 
                                StrokeColor = PdfCanvasColor.Registration }
                      )
                    ) 

                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.BottomMiddle (0., mm 3.2),
                      Position.BottomMiddle (0., 0.),
                      (fun args ->
                          { args with StrokeColor = PdfCanvasColor.Registration }
                      )
                    ) 

                    PageModifier.AddLine(
                      AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                      Position.BottomMiddle (mm -3.5, mm 3.2),
                      Position.BottomMiddle (mm 3.5, mm 3.2),
                      (fun args ->
                          { args with StrokeColor = PdfCanvasColor.Registration }
                      )
                    ) 

                  ]

                )
        )
        |> runTest "datas/manipulate/add line to position.pdf" 
        |> ignore


    testCase "add edge cropLines" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.AddEdgeCropMarks()
        )
        |> runTest "datas/manipulate/add edge cropLines.pdf" 
        |> ignore

    testCase "add colored texts to position" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "你好天气很好"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.CYAN }

                        { Text = "M"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA }

                        { Text = "是的啊"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.YELLOW }

                        { Text = "Unicode"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.BLACK }
                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable 
                                (RegisterableFonts.YaHei.yaHei RegisterableFonts.YaHei.FontWeight.Light)
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position.pdf" 
        |> ignore

    testCase "add colored texts to position2" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position2",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "BLACK"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.CYAN }

                        { Text = "C= 8.0, M=100.0, Y=15.0, K=0.0"
                          Color = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA }

                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            HorizontalTextAlignment = Some iText.Layout.Properties.TextAlignment.RIGHT
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position2.pdf" 
        |> ignore

    testCase "add colored texts to position3" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position3",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "BLACK"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.BLACK) }

                        { Text = "BLUE"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.BLUE) }
                           
                        { Text = "GRAY"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.GRAY) }
                           
                        { Text = "GREEN"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.GREEN) }
                           
                        { Text = "MAGENTA"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.MAGENTA) }
                           
                        { Text = "RED"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.RED) }
                           
                        { Text = "WHITE"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.WHITE) }
                           
                        { Text = "YELLOW"
                          Color = PdfCanvasColor.Value (FsValueColor.Rgb FsDeviceRgb.YELLOW) }

                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            HorizontalTextAlignment = Some iText.Layout.Properties.TextAlignment.RIGHT
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position3.pdf" 
        |> ignore


    testCase "add colored texts to position4" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add colored texts to position4",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddColoredTexts(
                    AreaGettingOptions.PageBox PageBoxKind.ActualBox,
                    [ 
                        { Text = "BLACK"
                          Color = PdfCanvasColor.Value (FsValueColor.Gray FsGray.BLACK) }

                        { Text = "GRAY"
                          Color = PdfCanvasColor.Value (FsValueColor.Gray FsGray.GRAY) }

                        { Text = "WHITE"
                          Color = PdfCanvasColor.Value (FsValueColor.Gray FsGray.WHITE) }

                        { Text = "CMYK__"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.BLACK) }

                        { Text = "BLACK"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.BLACK) }

                        
                        { Text = "CYAN"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.CYAN) }

                        { Text = "GRAY"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.GRAY) }

                        { Text = "GREEN"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.GREEN) }

                        { Text = "MAGENTA"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.MAGENTA) }

                        { Text = "RED"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.RED) }

                        { Text = "WHITE"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.WHITE) }

                        { Text = "YELLOW"
                          Color = PdfCanvasColor.Value (FsValueColor.Cmyk FsDeviceCmyk.YELLOW) }



                    ],
                    fun args -> 
                        { args with 
                            Position = Position.Center(0., 0.)
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            HorizontalTextAlignment = Some iText.Layout.Properties.TextAlignment.RIGHT
                        }
                  )
                )
        )
        |> runTest "datas/manipulate/add colored texts to position4.pdf" 
        |> ignore

    testCase "add text to position" <| fun _ -> 
        let longText = 
            "Arial"
            |> List.replicate 10
            |> String.concat " "

        let font = RegisterableFonts.Arial.arial RegisterableFonts.Arial.FontWeight.Italic

        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.Batch [
                    PageModifier.AddText(AreaGettingOptions.Specfic(Rectangle.create 0 0 100 100), longText, fun args ->
                      { args with 
                          PdfFontFactory = FsPdfFontFactory.Registerable font
                          CanvasFontSize = CanvasFontSize.Numeric 25. 
                          FontColor = PdfCanvasColor.Separation (FsSeparation.Create("帖标",FsValueColor.RGB_BLUE))
                          FontRotation = Rotation.None 
                          Position = Position.LeftMiddle(0., 0.)
                          ClipContents = true
                        }
                    )

                    PageModifier.AddText(PageBoxKind.ActualBox, "你好Separation", fun args ->
                      { args with 
                          
                          PdfFontFactory = FsPdfFontFactory.Registerable (CommonFonts.Songti)
                          CanvasFontSize = CanvasFontSize.Numeric 25. 
                          FontColor = PdfCanvasColor.Separation (FsSeparation.Create("帖标",FsValueColor.RGB_BLUE))
                          FontRotation = Rotation.None 
                          Position = Position.LeftTop(0., 0.)}
                    )

                    PageModifier.AddText(PageBoxKind.ActualBox, "你好LAB", fun args ->
                      { args with 
                          PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                          CanvasFontSize = CanvasFontSize.Numeric 25. 
                          FontColor = PdfCanvasColor.Lab {L = 50.f; a = 50.f; b = 50.f}
                          FontRotation = Rotation.None 
                          Position = Position.TopMiddle(0., 0.)}
                    )

                    PageModifier.AddText(
                        PageBoxKind.ActualBox,
                        "咬口正",
                        (fun args ->
                            { args with 
                                Position = Position.BottomMiddle (mm -5., mm 0.)
                                PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Regular)
                                //CanvasFontSize = CanvasFontSize.Numeric 8. 
                                //FontColor = PdfCanvasColor.Registration
                                //HorizontalTextAlignment = Some TextAlignment.RIGHT
                            }
                        )
                    )
                  ]
                )
        )
        //|> runTest "datas/bigData/#2021-11-01#(32073, 32075, 32076, 32077, 33149, 33150) 外箱.pdf" 
        |> runTest "datas/manipulate/add text to position.pdf" 
        |> ignore


    testCase "read texts2" <| fun _ ->

        let pdfFile = PdfFile @"datas/manipulate/read texts2.pdf"

        let infos = PdfRunner.ReadTextInfos_Record(pdfFile)

        ()

    testCase "read texts" <| fun _ ->

        let pdfFile = PdfFile @"datas/manipulate/read texts.pdf"

        let infos = PdfRunner.ReadTextInfos_Record(pdfFile)

        ()


    testCase "add text to position with font rotation" <| fun _ -> 
        let x = 
            List.replicate 55 "5"
            |> String.concat ""

        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,

                  PageModifier.Batch [
                    fun args ->
                        PageModifier.AddText(PageBoxKind.ActualBox, x + " @ 1 / 1 @", fun textAddingArgs ->
                          let pageSize =
                            args.Page.GetActualBox()
                            |> FsSize.ofRectangle

                          match pageSize with 
                          | FsSize.Uniform 
                          | FsSize.Landscape ->
                              { textAddingArgs with 
                                  PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                                  FontRotation = Rotation.Counterclockwise
                                  Position = Position.LeftTop(mm 0., mm -20.)}

                          | FsSize.Portrait ->
                            { textAddingArgs with 
                                PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                                FontRotation = Rotation.None
                                Position = Position.BottomMiddle(mm 0., mm 0.)}

                        ) args
                  ]
                )
        )
        //|> runTest "datas/bigData/#2021-11-01#(32073, 32075, 32076, 32077, 33149, 33150) 外箱.pdf" 
        |> runTest "datas/manipulate/add text to position with font rotation.pdf" 
        |> ignore

    testCase "add text to position with cached fonts" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts1",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(0., 0.)}
                  ) 
                )
        )
        <+>
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts2",
                  PageSelector.All,
                  Dummy,
                  fun args ->
                      PageModifier.AddText(PageBoxKind.ActualBox, "你好2", fun args ->
                        { args with 
                            PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                            CanvasFontSize = CanvasFontSize.Numeric 25. 
                            FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                            FontRotation = Rotation.None 
                            Position = Position.LeftTop(mm 20., 0.)}
                      ) args
                )
        )
        <+>
        Flow.Reuse (
            Reuse.dummy()
        )
        <+>
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position with cached fonts3",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好3", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(mm 40., 0.)}
                  ) 
                )
        )
        |> runTest "datas/manipulate/add text to position with cached fonts.pdf" 
        |> ignore

    testCase "add text to position and trimToVisible" <| fun _ -> 
        Flow.Manipulate (
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(0., 0.)}
                  ) 
                )
            <+>
            ModifyPage.TrimToVisible(PageSelector.All, Margin.Create 6.)
            <+>
            ModifyPage.Create
                ("add text to position",
                  PageSelector.All,
                  Dummy,
                  PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                    { args with 
                        PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                        CanvasFontSize = CanvasFontSize.Numeric 25. 
                        FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                        FontRotation = Rotation.None 
                        Position = Position.LeftTop(mm 3., 0.)}
                  ) 
                )

        )

        |> runTest "datas/manipulate/add text to position and trimToVisible.pdf" 
        |> ignore
    
    testCase "add page-scaling text" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "add page-scaling text",
                PageSelector.All,
                Dummy,
                PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                { args with 
                    PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                    CanvasFontSize = CanvasFontSize.OfRootArea 0.8 
                    FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                    FontRotation = Rotation.None 
                    Position = Position.Center(0., 0.)}
                )
            )
        )
        |> runTest "datas/manipulate/add page-scaling text.pdf" 
        |> ignore

    testCase "calc text line width" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "calc text line width",
                PageSelector.All,
                Dummy,
                (fun args infos ->
                    let fsFont = FsPdfFontFactory.Registerable (yaHei FontWeight.Regular)
                    let font = 
                        (args.Page.GetDocument() :?> PdfDocumentWithCachedResources)
                            .GetOrCreatePdfFont(fsFont)

                    let area = Rectangle.create 0. 0. 414. 68.

                    let text = "PANTONE 7407 C"

                    let fontSize = PdfFont.fontSizeOfArea (area) text font

                    let canvas =  new Canvas(args.Page, args.Page.GetActualBox())
                    let args =
                        { CanvasAddTextArguments.DefaultValue with 
                            CanvasFontSize = CanvasFontSize.Numeric 48.83
                            PdfFontFactory = fsFont
                                
                        }
                    let width = canvas.CalcTextWidth(text, args)
                    ()
                )
            )
        )
        |> runTest "datas/manipulate/calc text line width.pdf" 
        |> ignore

    testCase "rotate page and add text to top left" <| fun _ -> 
        Flow.Reuse(
            Reuses.Rotate(PageSelector.All, Rotation.Counterclockwise)
        )
        <+>
        Flow.Manipulate(
            ModifyPage.Create(
                "rotate page and add text to top left",
                PageSelector.All,
                Dummy,
                PageModifier.AddText(PageBoxKind.ActualBox, "你好", fun args ->
                { args with 
                    PdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)
                    CanvasFontSize = CanvasFontSize.Numeric 12.
                    FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                    FontRotation = Rotation.None 
                    Position = Position.LeftTop(0., 0.)}
                )
            )
        )
        |> runTest "datas/manipulate/rotate page and add text to top left.pdf" 
        |> ignore

    testCase "add page-scaling multiLines-text" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "setPageBox",
                PageSelector.All,
                Dummy,
                PageModifier.SetPageBox (PageBoxKind.AllBox, Rectangle.create 0. 0. 500. 100.)
            )
            <+>
            ModifyPage.Create(
                "add page-scaling multiLines-text",
                PageSelector.All,
                Dummy,
                fun args ->

                    let pdfFontFactory = FsPdfFontFactory.Registerable (yaHei FontWeight.Bold)

                    let text = "你好你好\n你好\n你好"

                    let canvasRootArea = 
                        let doc = args.Page.GetDocument() :?> PdfDocumentWithCachedResources
                        let pageBox = args.Page.GetPageBox(PageBoxKind.ActualBox)

                        let height = pageBox.GetHeightF()

                        let lineWidth =
                            let pdfFont = doc.GetOrCreatePdfFont(pdfFontFactory)
                            PdfFont.calcLineWidthWhenParagraphedHeightIs height text pdfFont

                        let x = pageBox.GetRightF() - (lineWidth * 0.8)
                        let y = pageBox.GetBottomF()

                        Rectangle.create x y (lineWidth * 0.8) height

                    PageModifier.AddText(
                        canvasRootArea,
                        text,
                        fun args ->
                            { args with 
                                PdfFontFactory = pdfFontFactory
                                CanvasFontSize = 
                                    CanvasFontSize.OfRootArea 0.8
                                FontColor = PdfCanvasColor.OfITextColor DeviceCmyk.MAGENTA 
                                FontRotation = Rotation.None 
                                Position = Position.LeftMiddle (0., 0.)}
                    ) args
            ) 
        )
        |> runTest "datas/manipulate/add page-scaling multiLines-text.pdf" 
        |> ignore

    testCase "add rect to area" <| fun _ -> 
        Flow.Manipulate(
            ModifyPage.Create(
                "add rect to area",
                PageSelector.All,
                Dummy,
                PageModifier.AddRectangleToCanvasRootArea(AreaGettingOptions.PageBox PageBoxKind.ActualBox, fun args -> { args with FillColor = NullablePdfCanvasColor.OfPdfCanvasColor(PdfCanvasColor.OfITextColor DeviceRgb.BLACK)})
            ) 
        )
        |> runTest "datas/manipulate/add rect to area.pdf" 
        |> ignore

    testCase "add marks to area" <| fun _ -> 
        let marks =
            [
                { Mark = Mark.LeftTauge; Position = Position.LeftBottom (0, mm 9.2)}
                { Mark = Mark.RightTauge; Position = Position.RightBottom (0, mm 9.2)}
                { Mark = Mark.VerticalRegistering; Position = Position.LeftMiddle(0, 0)}
                { Mark = Mark.VerticalRegistering; Position = Position.RightMiddle(0, 0)}
            ]

        Flow.Manipulate(
            ModifyPage.AddMarks(AreaGettingOptions.PageBox PageBoxKind.ActualBox, marks)
        )
        |> runTest "datas/manipulate/add marks to area.pdf" 
        |> ignore

    testList "trim to visible tests" [
        testCase "trim to visible test" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible.pdf" 
            |> ignore

        testCase "trim to visible test 2" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible(PageSelector.All, (Margin.Create(mm 6.)))
            )
            |> runTest "datas/manipulate/trim to visible2.pdf" 
            |> ignore

        testCase "trim to visible test 3" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible(PageSelector.All, (Margin.Create(mm 6.)))
            )
            |> runTest "datas/manipulate/trim to visible3.pdf" 
            |> ignore

        testCase "trim to visible test 4" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible(PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible4.pdf" 
            |> ignore

        testCase "trim to visible test5" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible5.pdf" 
            |> ignore

        testCase "trim to visible test6" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible6.pdf" 
            |> ignore

 

        testCase "trim to visible test7" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible7.pdf" 
            |> ignore

        testCase "trim to visible test8" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible8.pdf" 
            |> ignore
        
        testCase "trim to visible test9" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible9.pdf" 
            |> ignore
    
        testCase "trim to visible test10" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible10.pdf" 
            |> ignore


        testCase "trim to visible test11" <| fun _ -> 
            Flow.Manipulate(
                ModifyPage.TrimToVisible (PageSelector.All)
            )
            |> runTest "datas/manipulate/trim to visible11.pdf" 
            |> ignore
    ]



    testCase "test tissue infos" <| fun _ -> 
        let flow =
            ModifyPage.Create(
                "trim to visible",
                PageSelector.All,
                Path (Info.FillColorIs FsColor.CMYK_MAGENTA),
                (fun args renderInfos ->
                    let infos = List.ofSeq renderInfos
                    let m =     
                        infos
                        |> List.filter(IIntegratedRenderInfo.isFillVisible)

                    ()
                )
            )

        Flow.Manipulate(
            flow
        )
        |> runTest @"C:\Users\Administrator\Desktop\页面提取自－2_ExtractToTwoPages222.pdf"
        |> ignore

    testCase "map arial to arial_bold" <| fun _ -> 
        let flow =
            Modify.MapFontAndSize(
                FontAndSizeQuery([ArialMT], 12.) =>
                NewFontAndSize(FsPdfFontFactory.Registerable(yaHei FontWeight.Regular), 12.)
            )

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/map arial to arial_bold.pdf" 
        |> ignore

    testCase "split textLine to words" <| fun _ -> 
        let flow =
            Modify.SplitTextLineToWords()

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/split textLine to words.pdf" 
        //|> runTest @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\123.pdf"
        |> ignore

    testCase "split textLine to words2" <| fun _ -> 
        let flow =
            Modify.SplitTextLineToWords()

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/split textLine to words2.pdf" 
        //|> runTest @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\123.pdf"
        |> ignore

    testCase "split textLine to words3" <| fun _ -> 
        let flow =
            Modify.SplitTextLineToWords()

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/split textLine to words3.pdf" 
        //|> runTest @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\123.pdf"
        |> ignore

    testCase "split textLine to words4" <| fun _ -> 
        let flow =
            Modify.SplitTextLineToWords()
            <+>
            ModifyPage.Create(
                "ReadTextInfos",
                PageSelector.All,
                Selector.Text(fun _ _ -> true),
                (fun args infos ->
                    infos
                    |> List.ofSeq
                    |> List.choose IIntegratedRenderInfo.asITextRenderInfo
                    |> List.map (fun m -> m.RecordValue)
                )
            )

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/split textLine to words4.pdf" 
        //|> runTest @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\123.pdf"
        |> ignore

    testCase "split textLine to words5" <| fun _ -> 
        let flow =
            Modify.SplitTextLineToWords()

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/split textLine to words5.pdf" 
        //|> runTest @"D:\VsCode\Workspace\Shrimp.Pdf\Shrimp.Pdf.Tests\datas\123.pdf"
        |> ignore


    testCase "map font for horizontal line" <| fun _ -> 

        let flow =
            Modify.SplitTextLineToWords()
            <+>
            Manipulate.Factory(fun flowModel doc ->
                doc.Value.CacheDocumentFonts()
                Modify.MapFontAndSize(
                    FontAndSizeQuery(textPattern = TextSelectorOrTransformExpr.Selector (TextSelector.EqualTo ("30"))) =>
                    NewFontAndSize(FsPdfFontFactory.CreateDocumentFont(FontNames.``Tahoma-Bold``), fontSize = 12., alignment = XEffort.Middle)
                )
            )


        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/map font for horizontal line.pdf" 
        |> ignore
    
    testCase "add background for selected text" <| fun _ ->  
        let flow =
            Modify.SplitTextLineToWords()
            <+>
            Manipulate.Factory(fun flowModel doc ->
                doc.Value.CacheDocumentFonts(PageSelector.All)
                Modify.Create_Record(
                    PageSelector.All,
                    [
                        { SelectorAndModifiersRecord.Name = "Add background for selection"
                          Selector = 
                            Text(TextInfo.TextContainsIC "30")
                          Modifiers = 
                            [ 
                                Modifier.AddBackground (BackgroundFile.Create @"datas/manipulate/star.pdf", PasteObjectSize.BySelection Margin.MM6) 
                                Modifier.SetFillColor(DeviceCmyk.CYAN)
                                Modifier.ChangeTextStyle(
                                    TextStyle(
                                        VectorStyle.OpacityIs(0.5f),
                                        newFontAndSize = NewFontAndSize(FsPdfFontFactory.CreateDocumentFont(FontNames.``Tahoma-Bold``), alignment = XEffort.Middle)
                                    )
                                )
                            ]
                        }
                    ]
                )
            )

        Flow.Manipulate(
            flow
        )
        |> runTest "datas/manipulate/add background for selected text.pdf" 
        |> ignore

    let tryColoredSizeText() =
        Modify.Create(
            PageSelector.All,
            [ 
                SelectorAndModifiers(
                    "TryColoredSizeText",
                    Selector.Text(
                        (
                            Info.BoundIsOutsideOf (AreaGettingOptions.PageBox PageBoxKind.TrimBox)
                            <||>
                            Info.BoundIsCrossOf(AreaGettingOptions.PageBox PageBoxKind.TrimBox)
                        )
                        <&&>
                        TextInfo.FPrasec (PageInfos.sizeParser())
                        <&&>
                        Info.BoundIsInsideOf (AreaGettingOptions.PageBox PageBoxKind.ActualBox)
                        <&&>
                        Info.IsFillVisible()
                    ),
                    [Modifier.SetFillColor(iText.Kernel.Colors.DeviceRgb.RED)]
                )
            ]
        )


    testCase "read vertical texts" <| fun _ -> 

        let pdfFile = 
            @"datas/manipulate/add bound to vertical text2.pdf" 
            |> PdfFile


        let textInfos = 
            PdfRunner.ReadTextInfos_Record(pdfFile)
            |> List.concat
            |> List.filter(fun m -> m.TextRotation <> Rotation.None)
        ()
        

    testCase "test infos" <| fun _ -> 
        let m = FsDeviceRgb.OfLoggingText_Raw @"RGB 81 0 0"
        let pdfFile = 
            @"D:\Users\Jia\Documents\MyData\Docs\2017\健耐\DARK\包装\23-8-8\F2259 条形码.pdf"
            //@"D:\VsCode\Workspace\Shrimp.Pdf.Enhancement\tests\Shrimp.Pdf.Enhancement.Tests\datas\flows\Tile pages by cropLine3.pdf"
            |> PdfFile


        let colors = PdfRunner.ReadTextInfos_Record(pdfFile)


        let a = 1

        let flow =
            ModifyPage.Create(
                "trim to visible",
                PageSelector.All,
                PathOrText (fun _ _ -> true),
                (fun args renderInfos ->
                    let infos = 
                        List.ofSeq renderInfos
                        |> List.choose (IIntegratedRenderInfo.asIPathRenderInfo)
                        |> List.map(fun m -> m.RecordValue)
                        |> List.map(fun m -> FsColor.OfItextColor m.StrokeColor)
                        |> FsColors.distinct

                    let m = 1
                    ()
                )
            )

        Flow.Manipulate(
            tryColoredSizeText()
        )
        |> runTest "datas/manipulate/testInfos.pdf" 
        |> ignore

    testCase "create separationColor with shading" <| fun _ -> 
      let r = 
          Modify.CreateSeparaColors(SeparaColor.Separation (FsSeparation.OfPantone PantoneColorEnum.``PANTONE 4975 C``))
          |> Flow.Manipulate 
          |> runTest "datas/manipulate/create separation color.pdf" 
      pass()

    testCase "create separationColor2 with separtion DeviceN " <| fun _ -> 
      let r = 
          Modify.CreateSeparaColors(SeparaColor.Separation (FsSeparation.OfPantone PantoneColorEnum.``PANTONE 4975 C``))
          |> Flow.Manipulate 
          |> runTest "datas/manipulate/create separation color2.pdf" 
      pass()

    testCase "create separationColor3 with separtion DeviceN" <| fun _ -> 
      let r = 
          Modify.CreateSeparaColors(SeparaColor.Separation (FsSeparation.OfPantone PantoneColorEnum.``PANTONE 4975 C``))
          |> Flow.Manipulate 
          |> runTest "datas/manipulate/create separation color3.pdf" 
      pass()

    testCase "create separationColor3 with gray DeviceN" <| fun _ -> 
      let r = 
          Modify.CreateSeparaColors(SeparaColor.Black)
          |> Flow.Manipulate 
          |> runTest "datas/manipulate/create separation color4.pdf" 
      pass()


  ]