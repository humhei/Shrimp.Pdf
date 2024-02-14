namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open Shrimp.Pdf
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open System
open Shrimp.FSharp.Plus




type SlimModifyPage =
    static member TrimToVisible(?margin) =
        let margin = defaultArg margin Margin.Zero
        SlimFlow(fun flowModel args infos pageSetter ->
            let visibleBound = 
                infos.AsList
                |> List.map(fun m -> m.VisibleBound)
                |> List.ofSeq

            match AtLeastOneList.TryCreate visibleBound with 
            | None -> 
                { Infos = infos 
                  WriterPageSetter = SlimWriterPageSetter.Ignore
                  UserState = () }
                   
            | Some bounds ->
                let rect = 
                    Rectangle.ofRectangles bounds
                    |> Rectangle.applyMargin margin

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
            { Infos = f args infos 
              UserState = ()
              WriterPageSetter = pageSetter
            }
        )
        |> SlimFlow.rename 
            "MapInfos" 
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
            "SetPageBox" 
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

    static member Func(f) =
        SlimFlowUnion.Func(
            f
        )
        
