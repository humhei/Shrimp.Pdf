module ReuseTests
open Expecto
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas.Parser.Data
open Shrimp.Pdf
open Shrimp.Pdf
open System.IO
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Extensions
open iText.Kernel.Colors
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open iText.Kernel.Pdf
open iText.Kernel.Font
open Shrimp.Pdf.Image
open Shrimp.Pdf.Colors
open iText.Kernel.Pdf.Canvas

type Colors =
    /// 橙色
    static member PdfExtractorTagColor = FsColor.CreateRGB(247, 147, 30) 

let reuseTests =
  testList "Reuse Tests" [
    
    testCase "add background tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.AddBackground(BackgroundFile.Create @"datas/reuse/backgroundFile.pdf")
        )
        |> runTest "datas/reuse/add background.pdf" 
        |> ignore

    testCase "assignToLayer" <| fun _ -> 

        Flow.Reuse (
            Reuses.AssignToLayer("MyLayer")
        )
        |> runTest "datas/reuse/assignToLayer.pdf" 
        |> ignore

    testCase "set background size" <| fun _ -> 
        Flow.Reuse (
            Reuses.SetBackgroundSize(
                PageSelector.All,
                FsSize.A3,
                position = Position.LeftMiddle(mm 20, 0)
            )
        )
        |> runTest @"datas/reuse/set background size to A3.pdf" 
        |> ignore

    testCase "add background image tests2" <| fun _ -> 
        let image = @"datas/reuse/add background image.jpg" 
        Flow.Reuse (
            Reuses.AddForegroundImage(
                BackgroundImageFile.Singleton (FsFullPath image),
                //shadowColor = NullablePdfCanvasColor.WHITE,
                //layerName =  BackgroundAddingLayerOptions.Create ("Cdr", "Jpg"),
                extGSState = FsExtGState.Fill_Difference(1.0f)
            )
        )
        |> runTest @"datas/reuse/add background image.pdf" 
        |> ignore



    testCase "add background image tests ai rgb" <| fun _ -> 
        let image = @"datas/reuse/add background image rgb.jpg" 
        Flow.Reuse (
            Reuses.AddForegroundImage(
                BackgroundImageFile.Singleton (FsFullPath image),
                //shadowColor = NullablePdfCanvasColor.WHITE,
                layerName =  
                    BackgroundAddingLayerOptions.Pdf (
                        currentLayerName = "Cdr",
                        backgroundLayerName = ("Jpg")
                    ), 
                backgroundPositionTweak = fun _ -> BackgroundPositionTweak.OfPageBox(mm 20, mm 20)
                //extGSState = FsExtGState.Fill_Difference(1.0f)
            )
        )
        |> runTest @"datas/reuse/add background image rgb.pdf" 
        |> ignore

    testCase "add background as layer" <| fun _ -> 
        //let m = BackgroundFile.Create @"C:\Users\Jia\Desktop\New Document1.pdf"
        //let m = BackgroundFile.Create @"C:\Users\Jia\Desktop\New Document1.pdf"
        //let m = BackgroundFile.Create @"C:\Users\Jia\Desktop\New Document1.pdf"
        //let a = m

        let file =
            @"D:\VsCode\Workspace\Shrimp.Workflow\src\CustomerSupply\tests\CustomerSupply.Tests\bin\Debug\netcoreapp3.1\datas\Disney\健乐内盒90×40mmExtractor.raw.pdf"
            //@"datas/reuse/add background as layer.background.pdf"
        Flow.Reuse (
            Reuses.AddBackgroundOrForeground(
                BackgroundFile.Create(
                    file
                ),
                BackgroundOrForeground.Foreground
        ))
        |> runTest @"D:\Users\Jia\Documents\MyData\Docs\2017\健乐\Disney\包装\23-10-19\.extract\Innerboxlabels-306481.pdf"
        |> ignore

    testCase "add foreground tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.AddForeground(BackgroundFile.Create @"datas/reuse/backgroundFile.pdf")
        )
        |> runTest "datas/reuse/add foreground.pdf" 
        |> ignore

    testCase "impose cell rotation tests" <| fun _ -> 

        Flow.Manipulate(Manipulate.dummy())
        <+>
        Flow.Reuse (
            Reuse.dummy()
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 35.; Height = mm 82.}
                        ColNums = [10]
                        RowNum = 5
                        Cropmark = Some { Cropmark.defaultValue with IsRevealedBetweenCells = false }
                        Background = Background.Size FsSize.A0
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                        HSpaceExes = Spaces [mm 3.; mm 20.]
                        VSpaceExes = Spaces [mm 3.; mm 20.]
                        UseBleed = true
                        IsRepeated = true
                    }
                ) ||>> fun imposingDocument -> imposingDocument.GetSheets()
        )
        |> runTest "datas/reuse/impose cell rotation.pdf" 
        |> ignore

    testCase "Clipping Contents To PageBox" <| fun _ -> 

        Flow.Reuse (
            Reuse.dummy()
            <+>
            Reuses.ClippingContentsToPageBox(PageBoxKind.TrimBox, Margin.Create (mm 1., mm 2., mm 3., mm 4.))
        )
        |> runTest "datas/reuse/Clipping Contents To PageBox.pdf" 
        |> ignore

    testCase "Clipping Contents To PageBox2" <| fun _ -> 

        Flow.Reuse (
            Reuses.ClippingContentsToPageBox(PageBoxKind.ActualBox, Margin.Create (mm -1.))
        )
        |> runTest "datas/reuse/Clipping Contents To PageBox2.pdf" 
        |> ignore

    testCase "impose for heavy data" <| fun _ -> 
        Flow.Reuse (
            Reuses.MergeTwoPages(useBleed = true, margin = Margin.MM6)
        )
        |> runTest "datas/reuse/impose for heavy data.pdf" 
        |> ignore

    testCase "preimpose" <| fun _ -> 
        let p = 
            [1..100]
            |> List.map(fun _ -> System.IO.Path.GetTempFileNameEx())
        let r = 
            [
                1..10
            ]
            |> List.map(fun _ ->
                PdfRunner.PreImpose_Repeated_One
                    {_ImposingArguments.DefaultValue 
                        with ColNums = [0]
                             RowNum = 0 
                             Background = Background.Size ({Width = mm 273.; Height = mm 193.})
                             Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(6.))
                             DesiredPageOrientation = DesiredPageOrientation.Portrait
                             DesiredSizeOp  = 
                                ({Width = mm 210.; Height = mm 148.})
                                |> Some
                        }
            )


        let k = r.[0].ImposingSheet.GetTableBound(FsPoint.Zero)
        ()


    testCase "imposing force onePage" <| fun _ -> 

        Flow.Reuse (
            Reuses.Impose_ForceOnePage
                (fun args ->
                    { args with 
                        ColNums = [4]
                        RowNum = 3
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.A4
                        IsRepeated = false
                        HSpaceExes = Spaces [mm 0.]
                        VSpaceExes = Spaces [mm 0.]
                        BleedDistance = BleedDistance.SpecificValue (mm 3)
                        //DesiredPageOrientation = DesiredPageOrientation.Portrait
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                    }
                ) ||>> fun imposingDocument -> imposingDocument.GetSheets()
        )
        |> runTest "datas/reuse/imposing force onePage.pdf" 
        |> ignore

    testCase "imposing N-UP tests" <| fun _ -> 

        Flow.Reuse (
            Reuse.dummy()
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                        ColNums = [4]
                        RowNum = 4
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size (FsSize.A0)
                        HSpaceExes = Spaces [Space.MiddleDashLine(mm 3., mm 1.5); Space.MiddleDashLine(mm 9., mm 3.)]
                        VSpaceExes = Spaces [mm 3.; mm 20.]
                        Sheet_PlaceTable = Sheet_PlaceTable.At (Position.Center(0., 0.))
                        UseBleed = true
                    }
                ) ||>> fun imposingDocument -> imposingDocument.GetSheets()
        )
        |> runTest "datas/reuse/Imposing N-UP.pdf" 
        |> ignore

    testCase "imposing N-UP3 tests" <| fun _ -> 

        Flow.Reuse (
            Reuse.dummy()
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                        ColNums = [4]
                        RowNum = 4
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.A0
                        HSpaceExes = Spaces [mm 3.; mm 9.]
                        VSpaceExes =  Spaces [mm 3.; mm 9.]
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 30., mm 40., mm 50., mm 60.))
                        UseBleed = true
                    }
                ) 
        )
        <+> Flow.Manipulate(
            ModifyPage.AddSpaceMiddleLines(RowOrColumn.Row,fun (pageNumber, rowNumber) ->
                match pageNumber.Value, rowNumber.Value with 
                | 1, 1  
                | 1, 2 -> Some (SpaceMiddleLine.DashLine(
                    edgeSolidLine = SpaceMiddleLine_EdgeSolidLine.DefaultValue,
                    edgeLength_PercentToMargin = 0
                    
                )) 
                | _ -> None
            )
            <+>
            ModifyPage.AddSpaceMiddleLines(RowOrColumn.Column, fun (pageNumber, rowNumber) ->
                match pageNumber.Value, rowNumber.Value with 
                | 1, 1  
                | 1, 2 -> Some (SpaceMiddleLine.DashLine(
                    edgeSolidLine = SpaceMiddleLine_EdgeSolidLine.DefaultValue,
                    edgeLength_PercentToMargin = 0
                )) 
                | _ -> None
            )
        )
        |> runTest "datas/reuse/Imposing N-UP3.pdf" 
        |> ignore

    testCase "imposing N-UP2 tests" <| fun _ -> 

        Flow.Reuse (
            Reuses.Rotate(
                PageSelector.All,
                Rotation.Counterclockwise
            )
            <+>
            Reuses.Impose
                (fun args ->
                    { args with 
                        DesiredSizeOp = Some { Width = mm 30.; Height = mm 40.}
                        ColNums = [6]
                        RowNum = 12
                        Cropmark = Some Cropmark.defaultValue
                        Background = Background.Size FsSize.MAXIMUN
                        Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6.))
                        CellRotation = CellRotation.R180WhenColNumIsEven
                    }
                )
        )
        |> runTest "datas/reuse/imposing N-UP2.pdf" 
        |> ignore



    testCase "create page template tests" <| fun _ -> 

        Flow.Reuse (
            Reuses.CreatePageTemplate()
        )
        |> runTest "datas/reuse/create page template.pdf" 
        |> ignore

    testCase "imposing N-UP4 tests" <| fun _ -> 
        let r = 
            Flow.Reuse (
                Reuses.ImposeVertically
                    (fun args ->
                        { args with 
                            ColNums = [5]
                            RowNum = 4
                            Cropmark = Some { Cropmark.defaultValue with IsRevealedBetweenCells = false }
                            Background = Background.Size FsSize.A0
                            HSpaceExes = Spaces [mm 6.; mm 9.]
                            VSpaceExes =  Spaces [mm 3.; mm 9.]
                            Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 30., mm 40., mm 50., mm 60.))
                            UseBleed = true
                        }
                    )
            )
            |> runTest "datas/reuse/imposing N-UP4.pdf" 
        ()

    testCase "imposing N-UP5 tests" <| fun _ -> 
        let r = 
            Flow.Reuse (
                Reuses.ImposeVertically
                    (fun args ->
                        { args with 
                            ColNums = [5]
                            RowNum = 5
                            Background = Background.Size FsSize.A0
                            Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                            UseBleed = true
                            IsRepeated = false
                        }
                    )
            )
            |> runTest "datas/reuse/imposing N-UP5.pdf" 
        () 

    testCase "imposing N-UP6 tests" <| fun _ -> 
        let r = 
            Flow.Reuse (
                Reuses.Impose
                    (fun args ->
                        { args with 
                            ColNums = [1]
                            RowNum = 2
                            Background = Background.Size FsSize.MAXIMUN
                            Cropmark = (Some Cropmark.defaultValue)
                        }
                    )
            )
            |> runTest "datas/reuse/imposing N-UP6.pdf" 
        () 

    testCase "imposing N-UP7 tests" <| fun _ -> 

        Flow.Reuse (
            Reuses.Impose
                (fun args ->
                    let args = 
                        { args with 
                            ColNums = [0]
                            RowNum = 0
                            Cropmark = Some Cropmark.defaultValue
                            Background = Background.Size ({ Width = mm 222; Height = mm 298 })
                            Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.MM6)
                            UseBleed = true
                        }
                    args
                ) ||>> fun imposingDocument -> imposingDocument.GetSheets()
        )
        |> runTest "datas/reuse/Imposing N-UP7.pdf" 
        |> ignore

    testCase "imposing Multiple sizes tests" <| fun _ -> 
        let r = 
            Flow.Reuse (
                Reuses.Impose
                    (fun args ->
                        { args with 
                            ColNums = [0]
                            RowNum = 0
                            Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.MM6)
                            Background = Background.Size FsSize.A4
                            Cropmark = (Some Cropmark.defaultValue)
                        }
                    )
            )
            |> runTest "datas/reuse/Imposing N-UP multiple sizes.pdf" 
        () 

    testCase "imposing N-UP Big data tests" <| fun _ -> 
        let r = 
            Flow.Manipulate(
                ModifyPage.ScaleContentsTo(PageSelector.All, fun rect ->
                    Rectangle.applyMargin (-Margin.MM3) rect
                )
            )
            <+>
            Flow.Reuse (
                Reuses.Impose
                    (fun args ->
                        { args with 
                            ColNums = [2]
                            RowNum = 5
                            Background = Background.Size FsSize.A0
                            Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                            UseBleed = true
                            Cropmark = Some Cropmark.defaultValue
                            IsRepeated = false
                        }
                    )
            )
            |> runTest "datas/reuse/Imposing N-UP Bigdata.pdf" 
        () 

    testCase "imposing stepAndRepeat tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                    ColNums = [2]
                    RowNum = 2
                    Cropmark = Some Cropmark.defaultValue
                    HSpaceExes =  Spaces [mm 3.; mm 9.]
                    VSpaceExes =  Spaces [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create (mm 0., mm 6., mm 9., mm 12.))
                    IsRepeated = true
                }
            )
        )
        |> runTest "datas/reuse/imposing stepAndRepeat.pdf" 
        |> ignore

    testCase "imposing stepAndRepeat2 tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 80.; Height = mm 50.}
                    Cropmark = Some Cropmark.defaultValue
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable (Margin.Create(mm 6.))
                    Background = 
                        let size =
                            { Width = FsSize.A4.Width + mm 12. 
                              Height = FsSize.A4.Height + mm 12. }
                        Background.Size size
                    IsRepeated = true
                }
            )
        )
        |> runTest "datas/reuse/imposing stepAndRepeat2.pdf" 
        |> ignore


    testCase "Imposing when use bleed and bleedBox bigger than actualbox" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 50.}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaceExes = Spaces [mm 3.; mm 9.]
                    VSpaceExes = Spaces [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 9.863, mm 11.201, 0., 0.))
                    UseBleed = true
                }
            )
        )
        |> runTest "datas/reuse/Imposing when use bleed and bleedBox bigger than actualbox.pdf" 
        |> ignore

    testCase "Imposing when cell roation is setted" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 50.; Height = mm 30.}
                    ColNums = [4]
                    RowNum = 4
                    Cropmark = Some Cropmark.defaultValue
                    Background = Background.Size FsSize.A0
                    HSpaceExes = Spaces (Space.MiddleDashLine(mm 3.))
                    VSpaceExes = Spaces [mm 3.; mm 9.]
                    Sheet_PlaceTable = Sheet_PlaceTable.Trim_CenterTable(Margin.Create(mm 6., mm 6., mm 6., mm 6.))
                    UseBleed = true
                }
            )
        )
        |> runTest "datas/reuse/Imposing when cell roation is setted.pdf" 
        |> ignore

    testCase "Imposing when backgroudFile is setted" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 30.; Height = mm 30.}
                    ColNums = [8]
                    RowNum = 11
                    Background = Background.File (BackgroundFile.Create "datas/reuse/backgroundFile.pdf")
                    HSpaceExes = Spaces [mm 4.]
                    VSpaceExes = Spaces [mm 3.]
                    Sheet_PlaceTable = Sheet_PlaceTable.At(Position.LeftTop(mm 9.863, mm -11.201))
                }
            )
        )
        |> runTest "datas/reuse/Imposing when backgroudFile is setted.pdf" 
        |> ignore

    testCase "Imposing when backgroudFile is setted2" <| fun _ -> 
        Flow.Reuse (
            Reuses.Impose(fun args ->
                { args with 
                    DesiredSizeOp = Some { Width = mm 30.; Height = mm 30.}
                    ColNums = [8]
                    RowNum = 11
                    Background = 
                        (BackgroundFile.Create "datas/reuse/backgroundFile_R90.pdf").Clockwise()
                        |> Background.File
                    HSpaceExes = Spaces [mm 4.]
                    VSpaceExes = Spaces [mm 3.]
                    Sheet_PlaceTable = Sheet_PlaceTable.At(Position.LeftTop(mm 9.863, mm -11.201))
                }
            )
        )
        |> runTest "datas/reuse/Imposing when backgroudFile is setted2.pdf" 
        |> ignore

    testCase "clockwise all pages" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages.pdf" 
        |> ignore

    testCase "clockwise all pages2" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages2.pdf" 
        |> ignore

    testCase "clockwise all pages3" <| fun _ -> 
        Flow.Reuse (Reuses.Rotate(PageSelector.All, Rotation.Clockwise))
        |> runTest "datas/reuse/clockwise all pages3.pdf" 
        |> ignore

    testCase "duplicate all pages 5x tests" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages(PageSelector.All, 5))
        |> runTest "datas/reuse/duplicate all pages 5x.pdf" 
        |> ignore

    testCase "duplicate all pages 15x tests" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages(PageSelector.All, 15))
        |> runTest "datas/reuse/duplicate all pages 15x.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create [1;1;1;3;4;5;8]))
        |> runTest "datas/reuse/duplicate pages by page num sequence.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence with emptyPages tests" <| fun _ -> 
        let sequence =
            [
                EmptablePageNumSequenceToken.Create 2
                EmptablePageNumSequenceToken.Create 3
                EmptablePageNumSequenceToken.EmptyPage
                EmptablePageNumSequenceToken.Create 1
            ]
            |> EmptablePageNumSequence.Create


        Flow.Reuse (Reuses.SequencePages (sequence))
        |> runTest "datas/reuse/duplicate pages by page num sequence with empty Pages.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests2" <| fun _ -> 
        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create [1;1;1;3;4;5;8]))
        |> runTest "datas/reuse/duplicate pages by page num sequence2.pdf" 
        |> ignore

    testCase "duplicate pages by page num sequence tests3" <| fun _ -> 
        let tokens1 = 
            [1, Rotation.Clockwise; 1, Rotation.Counterclockwise; 5, Rotation.Counterclockwise]
            |> List.map PageNumSequenceToken.PageNumWithRotation

        let tokens2 = 
            [1, Flip.HFlip; 5, Flip.VFlip]
            |> List.map PageNumSequenceToken.PageNumWithFlip

        let tokens3 = 
            [1, Rotation.Clockwise, Flip.HFlip]
            |> List.map PageNumSequenceToken.PageNumWithRotationAndFlip

        let tokens4 = 
            [1, Flip.HFlip, Rotation.Clockwise]
            |> List.map PageNumSequenceToken.PageNumWithFlipAndRotation


        Flow.Reuse (Reuses.SequencePages (PageNumSequence.Create (tokens1 @ tokens2 @ tokens3 @ tokens4)))
        |> runTest "datas/reuse/duplicate pages by page num sequence3.pdf" 
        |> ignore



    testCase "duplicate pages by copied num sequence tests3" <| fun _ -> 
        Flow.Reuse (Reuses.DuplicatePages (PageSelector.All, CopiedNumSequence.Create [15;15;15;15]))
        |> runTest "datas/reuse/duplicate pages by copied num sequence.pdf" 
        |> ignore


    testCase "trim to first stroke color" <| fun _ -> 
        let strokeColor = 
            FsColor.CreateRGB(0.498f, 0.616f, 0.725f)
        let pageBoxKind = PageBoxKind.AllBox
        Flow.Manipulate(
            ModifyPage.Create( 
                sprintf "Set %O to cuttingLineColors" pageBoxKind,
                PageSelector.All,
                Path (Info.StrokeColorIs strokeColor),
                (fun args infos -> 
                    let rects = 
                        infos
                        |> Seq.map (IAbstractRenderInfo.getBound BoundGettingStrokeOptions.WithoutStrokeWidth)
                        |> List.ofSeq

                    let maxY = 
                        rects 
                        |> List.map (fun m -> m.GetYF())
                        |> List.max
                            
                    let minX = 
                        rects 
                        |> List.map (fun m -> m.GetXF())
                        |> List.min

                    let rect = 
                        rects
                        |> List.filter(fun m -> m.GetYF() @= maxY)
                        |> List.minBy(fun m -> m.GetX())

                    if (rect.GetXF() @= minX)
                    then
                        PageModifier.SetPageBox(pageBoxKind, rect) args infos
                    else failwithf "Cannot trim to the leftTop item, the top rect is %A, minX is %f" rect minX 
                )
            )
        )
      
        |> runTest "datas/reuse/trim to first stroke color.pdf" 
        |> ignore

    testCase "move pagebox to origin tests" <| fun _ -> 

        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
        )
        |> runTest "datas/reuse/move pagebox to origin.pdf" 
        |> ignore

    testCase "move pagebox to origin tests2" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
        )
        |> runTest "datas/reuse/move pagebox to origin2.pdf" 
        |> ignore

    testCase "move pagebox to origin tests3" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
        )
        |> runTest "datas/reuse/move pagebox to origin3.pdf" 
        |> ignore

    testCase "clear dirty infos" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
        )
        |> runTest "datas/reuse/clear dirty infos.pdf" 
        |> ignore

    testCase "clear dirty infos2" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
        )
        |> runTest "datas/reuse/clear dirty infos2.pdf" 
        |> ignore

    testCase "resize pageSize to 7x4cm tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(
                pageSelector = PageSelector.First,
                pageResizingRotatingOptions = PageResizingRotatingOptions.Keep,
                pageResizingScalingOptions = PageResizingScalingOptions.Uniform,
                fSize =
                    (fun page pageNumber -> { Width = mm 70.; Height = mm 40. })
            )
        )
        |> runTest "datas/reuse/resize pageSize to 7x4cm.pdf" 
        |> ignore

    testCase "resize pageSize to 5.5x2.5cm tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(
                PageSelector.All,
                PageBoxKind.TrimBox,
                fSize =
                    (fun page pageNumber -> { Width = mm 55.; Height = mm 25. })
            )
        )
        |> runTest "datas/reuse/resize pageSize to 5.5x2.5cm.pdf" 
        |> ignore


    testCase "resize pageSize to 5x3cm by trimbox tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, PageBoxKind.ActualBox , { Width = mm 105.8; Height = mm 155. })
        )
        |> runTest "datas/reuse/resize pageSize to 7x4cm by trimbox.pdf" 
        |> ignore

    testCase "resize pageSize to 5x3.8cm by trimbox" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
            <+>
            Reuses.Resize(PageSelector.All, PageBoxKind.TrimBox , { Width = mm 50.; Height = mm 50. })
        )
        |> runTest "datas/reuse/resize pageSize to 5x3.8cm by trimbox.pdf" 
        |> ignore

    testCase "resize pageSize to 5x3.8cm by trimbox2" <| fun _ -> 
        Flow.Reuse (
            Reuses.ClearDirtyInfos(keepOriginPageBoxes = true)
            <+>
            Reuses.Resize(PageSelector.All, PageBoxKind.TrimBox , { Width = mm 50.; Height = mm 50. })
        )
        |> runTest "datas/reuse/resize pageSize to 5x3.8cm by trimbox2.pdf" 
        |> ignore

    testCase "resize pageSize to 29.7×21cm uniform tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Resize(PageSelector.All, PageResizingRotatingOptions.Keep, PageResizingScalingOptions.Uniform , FsSize.landscape {Width = mm 210.; Height = mm 297.})
        )
        |> runTest "datas/reuse/resize pageSize to 29.7×21cm uniform.pdf" 
        |> ignore

    testCase "scaling page 0.8 tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Scale(PageSelector.All, 0.8, 0.8)
        )
        |> runTest "datas/reuse/scaling page 0.8.pdf" 
        |> ignore
        
    testCase "insert pages tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.Insert("datas/reuse/insertPagesResource.pdf", PageSelector.All, PageInsertingOptions.AfterPoint)
        )
        |> runTest "datas/reuse/insertPages.pdf" 
        |> ignore

    testCase "insert pages tests for each 2" <| fun _ -> 
        Flow.Reuse (
            Reuses.InsertEx(
                "datas/reuse/insert pages tests for each 2 Resource.pdf",
                pageInsertingOptions = PageInsertingOptionsEx.BeforeForEach(2))
        )
        |> runTest "datas/reuse/insert pages tests for each 2.pdf" 
        |> ignore

    testCase "insert pages tests for each 2 test2" <| fun _ -> 
        Flow.Reuse (
            Reuses.InsertEx(
                "datas/reuse/insert pages tests for each 2 Resource test2.pdf",
                pageInsertingOptions = PageInsertingOptionsEx.BeforeForEach(2))
        )
        |> runTest "datas/reuse/insert pages tests for each 2 test2.pdf" 
        |> ignore

    testCase "insert pages tests for each 3" <| fun _ -> 
        Flow.Reuse (
            Reuses.InsertEx(
                "datas/reuse/insert pages tests for each 3 Resource.pdf",
                pageInsertingOptions = PageInsertingOptionsEx.BeforeForEach(1))
        )
        |> runTest "datas/reuse/insert pages tests for each 3.pdf" 
        |> ignore

    testCase "insert empty pages tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.InsertEmptyPagesToMultiple(4)
        )
        |> runTest "datas/reuse/insertEmptyPagesTo4X.pdf" 
        |> ignore

    testCase "insert empty pages tests 2X" <| fun _ -> 
        Flow.Reuse (
            Reuses.InsertEmptyPagesToMultiple(2)
        )
        |> runTest "datas/reuse/insertEmptyPagesTo2X.pdf" 
        |> ignore

    testCase "change page orientation tests" <| fun _ -> 
        Flow.Reuse (
            Reuses.ChangePageOrientation(PageSelector.All, PageOrientation.Landscape)
        )
        |> runTest "datas/reuse/change page orientation.pdf" 
        |> ignore

]