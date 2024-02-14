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



type SlimWriterPageBoxSetter =
    { Rect: Rectangle
      Origin: PageBoxOrigin option
      RemoveRotation: bool
      TrimBoxMargin: NegativeMargin option 
      Scale: option<float * float> }
with 
    member x.InvokePage(readerPage: PdfPage, writerPage: PdfPage, writer) =

        let rotation =
            match x.RemoveRotation with 
            | false -> None
            | true -> Some (readerPage.GetFsRotation())

        let isRotated =
            match rotation with 
            | None -> false
            | Some rotation ->
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
                    | None -> margin.Value
                    | Some rotation -> margin.Value.Rotate(rotation)

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

    static member Create(readerPage: PdfPage, ?rect, ?origin, ?removeRotation, ?trimBoxMargin, ?scale) =
        { 
            Rect   = defaultArg rect (readerPage.GetActualBox())
            Origin = origin
            RemoveRotation = defaultArg removeRotation false
            TrimBoxMargin = trimBoxMargin
            Scale = scale
        }

type SlimWriterPageSetter =
    { PageBoxSetter: SlimWriterPageBoxSetter option
      Index: int }
with 

    member writerPageSetter.GenerateWriterPageAndCanvas(readerPage: PdfPage, writer: PdfDocument) =
        let writerPage, writerCanvas = 
            let actualBox = readerPage.GetActualBox()

            let rec writeInPage (transforms: AffineTransformRecord list) =
                match transforms with 
                | [] ->
                    let writerPage = writer.AddNewPage(PageSize(actualBox))

                    let writerCanvas = new OffsetablePdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
                    writerPage, writerCanvas

                | transforms ->
                    let transform = 
                        transforms
                        |> List.reduce(fun a b -> a.Concatenate(b))

                    let writerPage = writer.AddNewPage(PageSize actualBox)

                    let writerCanvas = new OffsetablePdfCanvas(writerPage.GetContentStream(0), writerPage.GetResources(), writer)
                    let xobject = PdfFormXObject(actualBox)
                    PdfCanvas.useCanvas writerCanvas (fun writerCanvas ->
                        writerCanvas.AddXObject(xobject, transform)
                    ) |> ignore

                    writerPage, new OffsetablePdfCanvas(xobject, writer)

            let transforms = 
                match writerPageSetter.PageBoxSetter with 
                | None -> []
                | Some pageBoxSetter ->
                    let rotates = 
                        match pageBoxSetter.RemoveRotation with 
                        | true -> 
                            let rotation = readerPage.GetFsRotation()
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

                                let offset2 =
                                    AffineTransformRecord.ofAffineTransform(
                                        AffineTransform.GetTranslateInstance(
                                        -actualBox.GetXF(),
                                        -actualBox.GetYF())
                                    )
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
                                

                        | false -> []

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


                    scale @ rotates @ Option.toList offset

            writeInPage transforms

        writerPage, writerCanvas

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

type SlimFlowResult<'userState> =
    { Infos: RenewableInfos 
      WriterPageSetter: SlimWriterPageSetter
      UserState: 'userState }
with 
    member x.MapUserState(f) =
        { Infos            = x.Infos 
          WriterPageSetter = x.WriterPageSetter
          UserState = f x.UserState }

type SlimFlowFunc<'userState, 'newUserState> = FlowModel<'userState> -> PageModifingArguments<'userState> -> RenewableInfos ->  SlimWriterPageSetter -> SlimFlowResult<'newUserState>

type SlimFlow<'userState, 'newUserState>(f: FlowModel<'userState> -> PageModifingArguments<'userState> -> RenewableInfos ->  SlimWriterPageSetter -> SlimFlowResult<'newUserState>, ?flowName: FlowName) =
    member internal x.SetFlowName(flowName: FlowName) =
        SlimFlow(f, flowName)


    member x.FlowName = flowName

    member internal x.Invoke (flowModel: FlowModel<'userState>) args infos setter = 
        match flowName with 
        | None -> f flowModel args infos setter
        | Some flowName ->  
            match args.PageNum with 
            | 1 -> 
                let flowModel1: InternalFlowModel<'userState> =  
                    let m = flowModel
                    { File = m.PdfFile.Path 
                      UserState = m.UserState 
                      FlowName = x.FlowName
                      OperatedFlowNames = []
                      Configuration =m.Configuration }


                let r = PdfLogger.TryInfoWithFlowModel(setter.Index, flowModel1, fun () -> f flowModel args infos setter)
                { r with WriterPageSetter = { r.WriterPageSetter with Index = r.WriterPageSetter.Index + 1} }

            | _ -> f flowModel args infos setter

    member internal x.SetParentFlowName(parentName: FlowName) =
        match x.FlowName with 
        | None -> x
        | Some flowName ->
            let newFlowName =  flowName.SetParentFlowName(parentName)
            x.SetFlowName(newFlowName)

    static member rename name parameters =
        fun (slimFlow: SlimFlow<'userState, 'newUserState>) ->
            SlimFlow(slimFlow.Invoke, FlowName.Override(name, parameters = parameters))
