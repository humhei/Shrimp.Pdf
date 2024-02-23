﻿namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open Shrimp.Pdf.icms2.client

open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open Shrimp.FSharp.Plus
open Shrimp.Pdf.Colors




type SlimModifyPage =

    static member TrimToVisible(?margin) =
        let margin = defaultArg margin Margin.Zero
        SlimFlow(fun flowModel args infos pageSetter ->
            let visibleBound = 
                infos.VisibleBound1()

            match visibleBound with 
            | None -> 
                { Infos = infos 
                  WriterPageSetter = SlimWriterPageSetter.Ignore
                  UserState = () }
                   
            | Some rect ->
                { Infos = infos 
                  UserState = ()
                  WriterPageSetter = 
                    { pageSetter with 
                        PageBoxSetter =
                            match pageSetter.PageBoxSetter with 
                            | None ->
                                Some 
                                    {
                                        Rect = rect
                                        Origin = None
                                        RemoveRotation = false
                                        TrimBoxMargin = None
                                        Scale = None
                                        Rotation = Rotation.None
                                    }

                            | Some pageBoxSetter ->
                                { pageBoxSetter with Rect = rect }
                                |> Some


                    }
                }
        )
        |> SlimFlow.rename 
            "TrimToVisible" 
            [ "margin" => margin.LoggingText ]
        |> SlimFlowUnion.Flow

    static member internal AddBackgroundOrForeground(background: SlimBackgroundUnion) =
        SlimFlow(fun flowModel args infos pageSetter ->
            let actualBox = args.Page.GetActualBox()
            { Infos = 
                { infos with
                    Background = 
                        let background = (background.GetByPageNumber(PageNumber args.PageNum)) 
                        let xEffect = defaultArg background.XEffect XEffort.Middle
                        let yEffect = defaultArg background.YEffect YEffort.Middle
                        let offsetX =
                            match xEffect with 
                            | XEffort.Left -> actualBox.GetXF()
                            | XEffort.Middle -> 
                                let widthDiff = background.PageBox.GetWidthF() - actualBox.GetWidthF() 
                                actualBox.GetXF() - widthDiff / 2.

                            | XEffort.Right ->
                                let widthDiff = background.PageBox.GetWidthF() - actualBox.GetWidthF() 
                                actualBox.GetXF() - widthDiff

                        let offsetY =
                            match yEffect with 
                            | YEffort.Bottom -> actualBox.GetYF()
                            | YEffort.Middle -> 
                                let heightDiff = background.PageBox.GetHeightF() - actualBox.GetHeightF() 
                                actualBox.GetYF() - heightDiff / 2.

                            | YEffort.Top ->
                                let heightDiff = background.PageBox.GetHeightF() - actualBox.GetHeightF() 
                                actualBox.GetYF() - heightDiff


                        background.ModifyInfos("FixVisibleBound", [], fun infos ->
                            infos
                            |> List.map(fun info0 ->
                                match info0 with 
                                | RenewableInfo.Path info ->
                                    match info.OriginInfo.LazyVisibleBound0 with 
                                    | None -> info0
                                    | Some bound ->
                                        let newBound = 
                                            bound.MapCoordinate(fun point ->
                                                { X = point.X + offsetX
                                                  Y = point.Y + offsetY 
                                            })
                                        { info with 
                                            OriginInfo.LazyVisibleBound0_Backup = 
                                                Some { Rectangle = bound; OffsetX = offsetX; OffsetY = offsetY }
                                            OriginInfo.LazyVisibleBound0 =
                                                Some newBound
                                        }
                                        |> RenewableInfo.Path

                                | RenewableInfo.Text info ->
                                    match info.OriginInfo.LazyVisibleBound0 with 
                                    | None -> info0
                                    | Some bound ->
                                        
                                        let newBound = bound.MapCoordinate(fun point ->
                                            { X = point.X + offsetX
                                              Y = point.Y + offsetY }
                                        )
                                        { info with 
                                            OriginInfo.LazyVisibleBound0_Backup = 
                                                Some { Rectangle = bound; OffsetX = offsetX; OffsetY = offsetY }
                                                
                                            OriginInfo.LazyVisibleBound0 =
                                                Some newBound
                                        }
                                        |> RenewableInfo.Text

                                | RenewableInfo.Image info ->
                                    match info.OriginInfo.LazyVisibleBound with 
                                    | None -> info0
                                    | Some bound ->
                                        let newBound = bound.MapCoordinate(fun point ->
                                            { X = point.X + offsetX
                                              Y = point.Y + offsetY }
                                        )
                                        { info with 
                                            OriginInfo.LazyVisibleBound_Backup = 
                                                Some { Rectangle = bound; OffsetX = offsetX; OffsetY = offsetY }
                                                
                                            OriginInfo.LazyVisibleBound =
                                                Some newBound
                                        }
                                        |> RenewableInfo.Image

                            )
                            |> List.map(fun m -> m.UpdateVisibleBound1())
                        )
                        let backgrounds = infos.Background @ [SolidableSlimBackground.SlimBackground background]

                        let slimBackgrounds = 
                            backgrounds
                            |> List.choose(fun m -> m.AsSlimBackground)


                        slimBackgrounds
                        |> List.choose(fun m -> m.LayerName)
                        |> List.ensureNotDuplicatedWith(fun m -> StringIC m.BackgroundLayer.Name)
                        |> ignore

                        slimBackgrounds
                        |> List.ensureNotDuplicatedWith(fun m -> m.KeyPath)
                        |> ignore

                        backgrounds


                } 
              WriterPageSetter = pageSetter 
              UserState = flowModel.UserState }
        )
        |> SlimFlow.rename 
            "AddBackgroundOrForeground" 
            [
                "bkNames" => String.concat "; " (background.FileNames()) 
                    
            ]
        |> SlimFlowUnion.Flow







    static member MovePageBoxToOrigin() =
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos 
              UserState = ()
              WriterPageSetter = 
                { pageSetter with 
                    PageBoxSetter =
                        match pageSetter.PageBoxSetter with 
                        | None -> 
                            {
                                Rect = args.Page.GetActualBox()
                                Origin = Some PageBoxOrigin.LeftBottom
                                RemoveRotation = false
                                TrimBoxMargin = None
                                Scale = None
                                Rotation = Rotation.None
                            }
                            |> Some
                        | Some pageBoxSetter ->
                            { pageBoxSetter with 
                                Origin = Some PageBoxOrigin.LeftBottom
                            }
                            |> Some
                }
            }
        )
        |> SlimFlow.rename 
            "MovePageBoxToOrigin" 
            []
        |> SlimFlowUnion.Flow


    static member ClearDirtyInfos() =
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos 
              UserState = ()
              WriterPageSetter = 
                { pageSetter with 
                    PageBoxSetter =
                        match pageSetter.PageBoxSetter with 
                        | None -> 
                            {
                                Rect = args.Page.GetActualBox()
                                Origin = Some PageBoxOrigin.LeftBottom
                                RemoveRotation = true
                                TrimBoxMargin = None
                                Scale = None
                                Rotation = Rotation.None
                            }
                            |> Some

                        | Some pageBoxSetter ->
                            { pageBoxSetter with 
                                Origin = Some PageBoxOrigin.LeftBottom
                                RemoveRotation = true
                            }
                            |> Some
                }
            }
        )
        |> SlimFlow.rename 
            "ClearDirtyInfos" 
            []
        |> SlimFlowUnion.Flow

    static member MapInfos(f) =
        SlimFlow(fun flowModel args infos pageSetter ->
            let infos =
                match infos.InternalFlowModel with 
                | None -> infos
                | Some flowModel ->
                    { infos with 
                        InternalFlowModel = 
                            flowModel.MapUserState(fun _ -> pageSetter.Index)
                            |> Some
                    }

            { Infos = f args infos 
              UserState = ()
              WriterPageSetter = pageSetter
            }
        )
        |> SlimFlow.rename 
            "MapInfos" 
            []
        |> SlimFlowUnion.Flow

    static member inPage pageSelector (flow: SlimFlowUnion<_, _>) =
        flow.InPage(pageSelector)

    static member InPage (pageSelector, flow: SlimFlowUnion<_, _>) =
        flow.InPage(pageSelector)
        
    static member InPageWith (pageSelector, flow: SlimFlowUnion<_, _>) =
        SlimModifyPage.Func2(fun flowModel args infos ->
            let flow = flow.InPage(pageSelector)
            flow

        )

    static member ReadInfos(name, f) =
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos
              UserState = f infos
              WriterPageSetter = pageSetter
            }
        )
        |> SlimFlow.rename 
            name 
            []
        |> SlimFlowUnion.Flow


    static member Scale(scaleX, scaleY) =
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos 
              UserState = ()
              WriterPageSetter = 
                { pageSetter with 
                    PageBoxSetter = 
                        match pageSetter.PageBoxSetter with 
                        | None -> 
                            SlimWriterPageBoxSetter.Create(args.Page, scale = (scaleX, scaleY))
                            |> Some

                        | Some pageBoxSetter -> 
                            { pageBoxSetter with Scale = Some (scaleX, scaleY)}
                            |> Some
                }
            }
        )
        |> SlimFlow.rename 
            "Scale" 
            []
        |> SlimFlowUnion.Flow

    static member SetPageBox(f) =
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos 
              UserState = ()
              WriterPageSetter = 
                { pageSetter with 
                    PageBoxSetter = 
                        match pageSetter.PageBoxSetter with 
                        | None -> failwithf "PageBox setter is None"
                        | Some pageBoxSetter -> Some (f pageBoxSetter)
                }
            }
        )
        |> SlimFlow.rename 
            "SetPageBox" 
            []
        |> SlimFlowUnion.Flow

    static member AddSolidBackgound(pageBoxKind: PageBoxKind, rectOps: PdfCanvasAddRectangleArguments -> PdfCanvasAddRectangleArguments, ?margin) =
        let args = rectOps PdfCanvasAddRectangleArguments.DefaultValue
        let background = 
            SolidSlimBackground(args, pageBoxKind, ?margin = margin)


        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = 
                { infos with Background = infos.Background @ [SolidableSlimBackground.Solid background] } 
              UserState = ()
              WriterPageSetter = pageSetter
            }
        )
        |> SlimFlow.rename 
            "AddSolidBackgrouond" 
            [
                "PageBoxKind" => pageBoxKind.Text()
                "RectOptions" => args.ToString()
                "Margin"      => margin.Text()
            ]
        |> SlimFlowUnion.Flow

    static member BlackOrWhite_Negative_Film(?strokeWidthIncrement: StrokeWidthIncrement, ?labIcc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParameters: NameAndParameters) =
        let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue
        let labIcc = defaultArg labIcc defaultLabIcc
        let inputIccFactory = options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)

        SlimModifyPage.AddSolidBackgound(
            PageBoxKind.ActualBox,
            fun args -> 
                { args with 
                    FillColor = NullablePdfCanvasColor.BLACK
                    StrokeColor = NullablePdfCanvasColor.N }
        )
        <+>
        SlimModifyPage.MapInfos(fun args infos ->
            let infos = 
                infos
                    .ReplaceAlternativeColors(
                        "BlackOrWhite_Negative_Film_ReplaceColors",
                        [],
                        fun color ->
                            match options.Predicate color with 
                            | true ->
                                color.Cms_AsBlackOrWhite_Inversed_FsColor(labIcc = labIcc, intent = options.Intent, ?inputIcc = inputIccFactory)
                                |> Some

                            | false -> None
                    )

            match strokeWidthIncrement with 
            | None -> infos
            | Some strokeWidthIncrement ->
                let parameters =
                    [
                        "strokeWidthIncrement" => strokeWidthIncrement.ToString()
                    ]
                    
                infos
                    .MapVector("BlackOrWhite_Negative_Film_ExpandStrokeWidth", parameters, fun info ->
                        info.ExpandStrokeWidth(
                            [FsColor.WHITE],
                            strokeWidthIncrement.Value,
                            PdfCanvasColor.WHITE,
                            lineJoinStyle = strokeWidthIncrement.LineJoinStyle
                        )
                    )

        )


    static member Func(f) =
        SlimFlowUnion.Func(
            f
        )

    static member Func2(f) =
        SlimFlowUnion.Func2(
            f
        )

[<RequireQualifiedAccess>]
module SlimModifyPage =
    let dummy() = 
        SlimFlow.dummy()
        |> SlimFlowUnion.Flow 
        
