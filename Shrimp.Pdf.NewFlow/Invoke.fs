namespace Shrimp.Pdf.SlimFlow
#nowarn "0104"
open System.Collections
open Shrimp.Pdf
open Shrimp.Pdf.PdfNames
open Shrimp.Pdf.Colors
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.DSL
open System.Collections.Generic
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Xobject
open iText.Kernel.Geom
open iText.Kernel.Pdf.Canvas
open System
open Shrimp.FSharp.Plus



type SlimWriterPageBoxSetter =
    { Rect: Rectangle
      Origin: PageBoxOrigin option
      RemoveRotation: bool
      TrimBoxMargin: NegativeMargin option 
      Scale: option<float * float>
      Rotation: Rotation }
with 
    member internal pageBoxSetter.GetAppliedRotation(readerPage: PdfPage) =
        let rotation = 
            match pageBoxSetter.RemoveRotation with 
            | false -> pageBoxSetter.Rotation
            | true -> 
                let rotation = readerPage.GetFsRotation()
                Rotation.concatenate rotation pageBoxSetter.Rotation

        rotation

    member x.InvokePage(readerPage: PdfPage, writerPage: PdfPage, writer) =

        let rotation = x.GetAppliedRotation(readerPage)

        let isRotated =
            match rotation with 
            | Rotation.None
            | Rotation.R180 -> false
            | Rotation.Clockwise
            | Rotation.Counterclockwise -> true


        let __setPageBox = 
            match x.Origin with 
            | None ->
                let rect =
                    match x.Scale with 
                    | None -> x.Rect
                    | Some (scaleX, scaleY) ->
                        AffineTransform.GetScaleInstance(scaleX, scaleY).Transform(x.Rect)

                writerPage.SetPageBox(PageBoxKind.AllBox, rect)
                |> ignore

            | Some origin ->
                let rect = 
                    match isRotated with 
                    | false -> Rectangle(x.Rect.GetWidth(), x.Rect.GetHeight())
                    | true -> Rectangle(x.Rect.GetHeight(), x.Rect.GetWidth())

                let rect =
                    match x.Scale with 
                    | None -> rect
                    | Some (scaleX, scaleY) ->
                        let width = rect.GetWidthF() * scaleX
                        let height = rect.GetHeightF() * scaleY
                        Rectangle(float32 width, float32 height)

                match origin with 
                | PageBoxOrigin.LeftBottom ->
                    writerPage.SetPageBox(PageBoxKind.AllBox, rect)
                    |> ignore


        let __setTrimBox = 
            match x.TrimBoxMargin with 
            | Some margin ->
                let margin = 
                    match rotation with 
                    | Rotation.None -> margin.Value
                    | rotation -> margin.Value.Rotate(rotation)

                let margin = 
                    match x.Scale with 
                    | None -> margin
                    | Some (scaleX, scaleY) -> 
                        //let halfWidth = (1. - scaleX) * (margin.Right - margin.Left) / 2.
                        //let halfHeight = (1. - scaleY) * (margin.Top - margin.Bottom) / 2.
                        //{ Left = margin.Left - halfWidth
                        //  Right = margin.Right + halfWidth
                        //  Top = margin.Top + halfHeight
                        //  Bottom = margin.Bottom - halfHeight }

                        { Left = margin.Left * scaleX 
                          Right = margin.Right * scaleX 
                          Top = margin.Top * scaleY
                          Bottom = margin.Bottom * scaleY }

                let pageBox = writerPage.GetActualBox()
                match x.Origin with 
                | None ->
                    let trimBox = Rectangle.applyMargin (margin) pageBox
                    writerPage.SetTrimBox(trimBox)
                    |> ignore

                | Some origin ->
                    match origin with 
                    | PageBoxOrigin.LeftBottom ->
                        let trimBox = Rectangle.applyMargin (margin) pageBox
                        writerPage.SetTrimBox(trimBox)
                        |> ignore

            | None -> ()



        ()

    static member Create(readerPage: PdfPage, ?rect, ?origin, ?removeRotation, ?trimBoxMargin, ?scale, ?rotation) =
        { 
            Rect   = defaultArg rect (readerPage.GetActualBox())
            Origin = origin
            RemoveRotation = defaultArg removeRotation false
            TrimBoxMargin = trimBoxMargin
            Scale = scale
            Rotation = defaultArg rotation Rotation.None
        }

type SlimWriterPageSetter =
    { PageBoxSetter: SlimWriterPageBoxSetter option
      Index: int }
with 

    member writerPageSetter.GenerateTransforms(readerPage: PdfPage) =
            match writerPageSetter.PageBoxSetter with 
            | None -> None
            | Some pageBoxSetter ->
                let rotates = 
                    let rotation = 
                        pageBoxSetter.GetAppliedRotation(readerPage)

                    match rotation with 
                    | Rotation.None -> []
                    | _ -> 
                        match rotation with 
                        | Rotation.None -> []
                        | _ -> 
                            let angle = Rotation.getAngle rotation
                            let offset1 =
                                match rotation with 
                                | Rotation.Clockwise -> 
                                    let rect = pageBoxSetter.Rect
                                    AffineTransformRecord.ofAffineTransform(
                                        AffineTransform.GetTranslateInstance(
                                        0,
                                        rect.GetWidthF())
                                    )
                                | Rotation.Counterclockwise -> 
                                    let rect = pageBoxSetter.Rect
                                    AffineTransformRecord.ofAffineTransform(
                                        AffineTransform.GetTranslateInstance(
                                        rect.GetHeightF(),
                                        0)
                                    )


                                | Rotation.R180 -> 
                                    let rect = pageBoxSetter.Rect
                                    AffineTransformRecord.ofAffineTransform(
                                        AffineTransform.GetTranslateInstance(
                                        rect.GetWidthF(),
                                        rect.GetHeightF())
                                    )
                                | Rotation.None -> failwithf "Invalid token" 

                            //let offset2 =
                            //    AffineTransformRecord.ofAffineTransform(
                            //        AffineTransform.GetTranslateInstance(
                            //        -actualBox.GetXF(),
                            //        -actualBox.GetYF())
                            //    )
                                //match rotation with 
                                //| Rotation.Clockwise -> 
                                //    AffineTransformRecord.ofAffineTransform(
                                //        AffineTransform.GetTranslateInstance(
                                //        -actualBox.GetXF(),
                                //        -actualBox.GetYF())
                                //    )
                                //| Rotation.Counterclockwise -> 
                                //    AffineTransformRecord.ofAffineTransform(
                                //        AffineTransform.GetTranslateInstance(
                                //        -actualBox.GetXF(),
                                //        -actualBox.GetYF())
                                //    )
                                //| _ -> failwithf "Not implemented"

                            [
                                offset1
                                AffineTransformRecord.ofAffineTransform(AffineTransform.GetRotateInstance(Math.PI / -180. * angle))
                                //offset2
                            ]
                            


                let offset =
                    match pageBoxSetter.Origin with 
                    | None -> None
                    | Some origin  ->
                        match origin with 
                        | PageBoxOrigin.LeftBottom ->
                            let point = FsPoint.OfPoint pageBoxSetter.Rect.LeftBottom
                            match point = { X = 0.; Y = 0.} with 
                            | true ->  None
                            | false ->
                                AffineTransform.GetTranslateInstance(-point.X, -point.Y)
                                |> AffineTransformRecord.ofAffineTransform
                                |> Some

                let scale =
                    match pageBoxSetter.Scale with 
                    | None -> []
                    | Some (scaleX, scaleY) ->
                        match pageBoxSetter.Origin with 
                        | None ->
                            AffineTransform.GetScaleInstance(scaleX, scaleY)
                            |> AffineTransformRecord.ofAffineTransform
                            |> List.singleton

                        | Some origin ->
                            match origin with 
                            | PageBoxOrigin.LeftBottom ->
                                let scale = 
                                    AffineTransform.GetScaleInstance(scaleX, scaleY)
                                    |> AffineTransformRecord.ofAffineTransform

                                [scale]

                                //match rotates with 
                                //| [] ->
                                //    let translate = 
                                //        AffineTransformRecord.ofAffineTransform(
                                //            AffineTransform.GetTranslateInstance(
                                //            -actualBox.GetXF(),
                                //            -actualBox.GetYF())
                                //        )

                                //    [scale; translate]

                                //| _ -> [scale]


                let transforms = scale @ rotates @ Option.toList offset
                transforms
                |> AtLeastOneList.TryCreate
                |> Option.map(fun transforms ->
                    transforms.AsList
                    |> List.reduce(fun a b -> a.Concatenate(b))
                )

    member writerPageSetter.GenerateWriterPageAndCanvas(readerPage: PdfPage, writer: PdfDocument, background: SolidableSlimBackground list, writeInfos) =
        let actualBox = readerPage.GetActualBox()

        let writerPage = writer.AddNewPage(PageSize(actualBox))
        let transforms = writerPageSetter.GenerateTransforms(readerPage)

        let writerCanvas = 
            let contentStream = writerPage.GetContentStream(0)

            let layerOptions =
                background
                |> List.choose(fun m -> m.AsSlimBackground)
                |> List.choose(fun m -> m.LayerName)
                |> List.tryLast

            match layerOptions with 
            | None -> 
                match background with 
                | [] -> ()
                | _ ->
                    contentStream.Put(PdfName.ShpLayerContent, PdfString "Content")
                    |> ignore

            | Some layerOptions -> 
                let shpLayerName = layerOptions.CurrentLayer.Name
                contentStream.Put(PdfName.ShpLayerContent, PdfString shpLayerName)
                |> ignore

            let writerCanvas = new OffsetablePdfCanvas(contentStream, writerPage.GetResources(), writer)
            
            match background with 
            | [] -> ()
            | backgrounds ->
                backgrounds
                |> List.iter(fun background ->
                    match background with 
                    | SolidableSlimBackground.SlimBackground background ->
                        let backgroundPositionTweak = 
                            background.BackgroundPositionTweak
                            |> Option.defaultValue ((fun _ -> BackgroundPositionTweak.DefaultValue) )
                            >> fun tweak ->     
                                let tweak = 
                                    match writerPageSetter.SetPageBox_Origin with 
                                    | None -> tweak
                                    | Some origin ->
                                        match origin with 
                                        | PageBoxOrigin.LeftBottom -> 
                                            let pageBox = writerPage.GetActualBox()
                                            tweak.Offset(-pageBox.GetXF(), -pageBox.GetYF())

                                let tweak = 
                                    match writerPageSetter.PageBoxSetter with 
                                    | None -> tweak
                                    | Some pageBoxSetter ->
                                        match pageBoxSetter.Scale with 
                                        | None -> tweak
                                        | Some scale -> tweak.SetScale(Some scale)

                                tweak


                        writerPage.AddBackgroundOrForegroundImage(
                            1,
                            new Concurrent.ConcurrentDictionary<_, _>(),
                            RenewableBackgroundImageFile.SlimBackground (writeInfos, background),
                            background.Choice,
                            ?layerName               = background.LayerName,
                            ?xEffect                 = background.XEffect,
                            ?yEffect                 = background.YEffect,
                            ?shadowColor             = background.ShadowColor,
                            ?extGSState              = background.ExtGsState,
                            backgroundPositionTweak  = backgroundPositionTweak
                        )

                    | SolidableSlimBackground.Solid solidBackground ->
                        let stream = writerPage.NewContentStreamBefore()

                        stream.Put(PdfName.ShpLayerBkSolid, PdfBoolean true)
                        |> ignore

                        let pdfCanvas = new OffsetablePdfCanvas(stream, writerPage.GetResources(), writer)
                        pdfCanvas.SaveState() |> ignore
                        let pageBox = writerPage.GetPageBox(solidBackground.PageBoxKind)

                        match transforms with 
                        | None -> ()
                        | Some transforms ->
                            PdfCanvas.concatMatrixByTransformRecord transforms pdfCanvas
                            |> ignore

                        PdfCanvas.addRectangle pageBox (fun _ -> solidBackground.RectOptions) pdfCanvas
                        |> ignore

                        pdfCanvas.RestoreState() |> ignore


                )



            let writeInPage (transforms: AffineTransformRecord option) =
                match transforms with 
                | None -> writerCanvas

                | Some transforms ->

                    let xobject = PdfFormXObject(actualBox)
                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas.AddXObject(xobject, transforms)
                    ) |> ignore

                    new OffsetablePdfCanvas(xobject, writer)


            writeInPage transforms



        writerPage, writerCanvas, transforms

    member x.IsIgnore = 
        x.PageBoxSetter.IsNone

    member x.SetPageBox_Origin =
        match x.PageBoxSetter with 
        | None -> None
        | Some setPageBox -> setPageBox.Origin

    static member Ignore = { PageBoxSetter = None; Index = 1 }

    member x.InvokePage(writerPage) =
        match x.PageBoxSetter with 
        | Some setPageBox -> setPageBox.InvokePage writerPage
        | None -> ()

[<RequireQualifiedAccess>]
module SlimWriterPageSetter =
    let (|Ignore|Some|) (setter: SlimWriterPageSetter) =
        match setter.IsIgnore with 
        | true -> Ignore ()
        | false -> Some setter


type CurrentSlimFlowCacheFactory() =
    let mutable isDiposed = false
    let dict = new System.Collections.Concurrent.ConcurrentDictionary<string, obj>()

    member x.GetOrCreate<'T>(name: string, fCache: string -> 'T) =
        dict.GetOrAdd(name, valueFactory = fun name ->
            box(fCache name)
        )
        |> unbox<'T>

    interface System.IDisposable with 
        member x.Dispose() =
            match isDiposed with 
            | false ->
                for pair in dict do
                    match pair.Value with 
                    | :? IDictionary as dict -> dict.Clear()
                    | :? System.IDisposable as disposable -> disposable.Dispose()
                    | _ -> ()

            | true -> ()



type SlimFlowResult<'userState> =
    { Infos: RenewableInfos 
      WriterPageSetter: SlimWriterPageSetter
      UserState: 'userState }
with 
    member x.MapUserState(f) =
        { Infos            = x.Infos 
          WriterPageSetter = x.WriterPageSetter
          UserState = f x.UserState }


type SlimFlowBackgroundFactory() =
    let dict = System.Collections.Concurrent.ConcurrentDictionary<FsFullPath list, SlimBackgroundUnion>()

    member internal x.Dict = dict

type SlimUserStatesCache<'userState>() =
    let userStates = System.Collections.Generic.Dictionary<PageNumber, 'userState>()
    
    member x.Add(pageNumber, userState) = userStates.Add(pageNumber, userState)

    interface System.IDisposable with 
        member x.Dispose() = userStates.Clear()

type SlimFlowModel<'userState, 'newUserState> =    
    { BackgroundFactory: SlimFlowBackgroundFactory
      FlowModel: FlowModel<'userState>
      CurrentSlimFlowCacheFactory: CurrentSlimFlowCacheFactory
      CurrentUserStateCache: SlimUserStatesCache<'newUserState> option
      AllSlimFlowCacheFactory: HashSet<System.IDisposable> }
with 

    member x.PdfFile = x.FlowModel.PdfFile

    member x.UserState = x.FlowModel.UserState

    member x.Configuration = x.FlowModel.Configuration

    member x.MapUserState(f) =
        { BackgroundFactory = x.BackgroundFactory 
          FlowModel = x.FlowModel.MapUserState f
          CurrentSlimFlowCacheFactory = x.CurrentSlimFlowCacheFactory
          AllSlimFlowCacheFactory = x.AllSlimFlowCacheFactory
          CurrentUserStateCache = x.CurrentUserStateCache }

    member internal x.SetUserStateCacheToNone() =
        { BackgroundFactory = x.BackgroundFactory 
          FlowModel = x.FlowModel
          CurrentSlimFlowCacheFactory = x.CurrentSlimFlowCacheFactory
          AllSlimFlowCacheFactory = x.AllSlimFlowCacheFactory
          CurrentUserStateCache = None }

type SlimFlowFunc<'userState, 'newUserState> = SlimFlowModel<'userState, 'newUserState> -> PageModifingArguments<'userState> -> RenewableInfos ->  SlimWriterPageSetter -> SlimFlowResult<'newUserState>


    

type SlimFlow<'userState, 'newUserState>(f: SlimFlowModel<'userState, 'newUserState> -> PageModifingArguments<'userState> -> RenewableInfos ->  SlimWriterPageSetter -> SlimFlowResult<'newUserState>, ?flowName: FlowName, ?background) =
    let userStates = new SlimUserStatesCache<'newUserState>()

    let cacheFactory = new CurrentSlimFlowCacheFactory()

    member x.UserStatesCache = userStates

    member internal x.SetFlowName(flowName: FlowName) =
        SlimFlow(f, flowName)

    member x.Background: SolidableSlimBackground option = background

    member x.FlowName = flowName

    member internal x.ClearCache() = 
        (cacheFactory :> System.IDisposable).Dispose()
        (userStates :> System.IDisposable).Dispose()
        

    member internal x.Internal_Invoke_Origin = f

    member internal x.Invoke (flowModel: SlimFlowModel<'userState, 'newUserState>) args infos setter = 
        let flowModel = 
            { flowModel with 
                CurrentSlimFlowCacheFactory = cacheFactory
                CurrentUserStateCache = Some userStates
            }

        flowModel.AllSlimFlowCacheFactory.Add(cacheFactory)
        |> ignore

        let f flowModel args infos pageSetter =
            let infos =
                match infos.InternalFlowModel with 
                | None -> infos
                | Some flowModel ->
                    { infos with 
                        InternalFlowModel = 
                            flowModel.MapUserState(fun _ -> pageSetter.Index)
                            |> Some
                    }

            f flowModel args infos pageSetter

        let result = 
            match flowName with 
            | None -> 
                f flowModel args infos setter

            | Some flowName ->  
                match args.PageNum with 
                | 1 -> 
                    let flowModel1: InternalFlowModel<'userState> =  
                        let m = flowModel
                        { File = m.PdfFile.Path 
                          UserState = m.UserState 
                          FlowName = x.FlowName
                          OperatedFlowNames = []
                          Configuration = m.Configuration }

                    let infos = 
                        { infos with InternalFlowModel = Some (flowModel1.MapUserState (fun _ -> -1)) }

                    let r = PdfLogger.TryInfoWithFlowModel(setter.Index, flowModel1, fun () -> f flowModel args infos setter)
                    { r with WriterPageSetter = { r.WriterPageSetter with Index = r.WriterPageSetter.Index + 1} }

                | _ -> f flowModel args infos setter

        userStates.Add(PageNumber args.PageNum, result.UserState)

        result

    member internal x.SetParentFlowName(parentName: FlowName) =
        match x.FlowName with 
        | None -> x
        | Some flowName ->
            let newFlowName =  flowName.SetParentFlowName(parentName)
            x.SetFlowName(newFlowName)


    static member rename name parameters =
        fun (slimFlow: SlimFlow<'userState, 'newUserState>) ->
            SlimFlow(slimFlow.Internal_Invoke_Origin, FlowName.Override(name, parameters = parameters))

[<RequireQualifiedAccess>]
module PageModifingArguments =
    let isCurrentPageSelected (pageSelector: PageSelector) (args: PageModifingArguments<_>) =
        match pageSelector with 
        | PageSelector.All -> true
            
        | _ ->
            let pageNumbers = 
                PdfDocument.GetPageNumbers_Static pageSelector args.TotalNumberOfPages

            List.contains args.PageNum pageNumbers

[<RequireQualifiedAccess>]
module SlimFlow =
    let dummy() = 
        SlimFlow(fun flowModel args infos pageSetter ->
            { Infos = infos 
              UserState = ()
              WriterPageSetter = pageSetter
            }
        )

    let mapStateBack (mapping) (x: SlimFlow<_, _>) =
        let newF (flowModel: SlimFlowModel<_, _>) (args: PageModifingArguments<_>) infos pageSetter =
            let flowModel = flowModel.MapUserState mapping
            let args = args.MapUserState mapping
            x.Internal_Invoke_Origin flowModel args infos pageSetter

        new SlimFlow<_, _>(newF, ?flowName = x.FlowName, ?background = x.Background)


    let mapState (mapping) (x: SlimFlow<_, _>) =
        let newF flowModel args infos pageSetter =
            let r = x.Internal_Invoke_Origin flowModel args infos pageSetter
            r.MapUserState(mapping)

        new SlimFlow<_, _>(newF, ?flowName = x.FlowName, ?background = x.Background)

    let applyPageSelector (pageSelector: PageSelector) (x: SlimFlow<_, _>) =
        let newF (flowModel: SlimFlowModel<_, _>) (args: PageModifingArguments<_>) infos pageSetter =
            match PageModifingArguments.isCurrentPageSelected pageSelector args with 
            | true -> 
                let flowModel = flowModel.SetUserStateCacheToNone()
                let r = x.Internal_Invoke_Origin flowModel args infos pageSetter
                r.MapUserState(Some)
                
            | false ->
                let r = 
                    { Infos = infos 
                      UserState = None
                      WriterPageSetter = pageSetter
                    }
                r



        new SlimFlow<_, _>(newF, ?flowName = x.FlowName, ?background = x.Background)
