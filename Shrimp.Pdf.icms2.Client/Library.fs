namespace Shrimp.Pdf.icms2
#nowarn "0104"
open Shrimp.Pdf.icms2.Core
open iText.Kernel.Colors
open Akka.Configuration
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open System
open Akkling
open System.Collections.ObjectModel
open Shrimp.Pdf.DSL
open Shrimp.Pdf
open Shrimp.Pdf.Extensions
open Shrimp.Pdf.Parser
open System.Collections.Concurrent
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Pdf.Colors

[<AutoOpen>]
module Client =

    type private AssemblyFinder = AssemblyFinder

    [<AutoOpen>]
    module ClientCluster =
        let  mutable private client: option<Client<unit, ServerMsg>> = None
        let SetClientContext() = client <- Some (Client.create())
        let GetClient() = 
            match client with
            | Some client -> client
            | None -> failwith "Please run SetClientContext() first to aceess icm server"



    let private referenceConfig = 
        ConfigurationFactory.FromResource<AssemblyFinder>("Shrimp.Pdf.icms2.Client.reference.conf")
        |> Configuration.fallBackByApplicationConf

    let private getEnumFromConfig configName : 'enum =
        let avaliableIccs = Enum.GetNames(typeof<'enum>)
        let cmykIccText = referenceConfig.GetString(configName)
        match Array.tryFind (fun m -> String.Compare(m, cmykIccText.Trim(), true) = 0) avaliableIccs with 
        | Some icc -> 
            Enum.Parse(typeof<'enum>, icc)
            |> unbox
        | None -> failwithf "avaliableIccs %A not include %s" avaliableIccs cmykIccText 
     
    let defaultCmykIcc: CmykIcc = 
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.cmyk"

    let defaultRgbIcc: RgbIcc =
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.rgb"


    let defaultLabIcc: LabIcc =
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.lab"

    let defaultGrayIcc: GrayIcc =
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.gray"

    let defaultIntent: Intent =
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.intent"

    let defaultMaxCmsGrayDeviation: float32 =
        referenceConfig.GetFloat("shrimp.pdf.icms2.client.maxCmsGrayDeviation")


    let private msgCache = new ConcurrentDictionary<ServerMsg, float32[]>()



    [<RequireQualifiedAccess>]
    type CmsColor =
        | Lab of FsLab
        | Cmyk of DeviceCmyk
        | Rgb of DeviceRgb
        | Gray of DeviceGray
        | Separation of Separation
    with 
        static member DefaultIcc = function
            | CmsColor.Rgb _ -> Icc.Rgb defaultRgbIcc
            | CmsColor.Cmyk _ -> Icc.Cmyk defaultCmykIcc
            | CmsColor.Gray _ -> Icc.Gray defaultGrayIcc
            | CmsColor.Lab _ -> Icc.Lab defaultLabIcc
            | CmsColor.Separation separation -> failwith "Not implement"
          
        member x.AsLab() =
            match x with 
            | CmsColor.Lab lab -> Some lab
            | _ -> None

        member x.GetColor() =
            match x with 
            | CmsColor.Lab lab -> lab.ToItextColor()
            | CmsColor.Cmyk cmyk -> cmyk :> Color
            | CmsColor.Gray gray -> gray :> Color
            | CmsColor.Rgb rgb -> rgb :> Color
            | CmsColor.Separation separation -> failwith "Not implement"

        member x.GetColorValues() =
            match x with 
            | CmsColor.Lab lab -> 
                [|lab.L; lab.a; lab.b|]
            | CmsColor.Cmyk cmyk -> cmyk.GetColorValue()
            | CmsColor.Gray gray -> gray.GetColorValue()
            | CmsColor.Rgb rgb -> rgb.GetColorValue()
            | CmsColor.Separation separation -> failwith "Not implement"

        member x.IsSameColorSpaceWith(icc: Icc) =
            match x, icc with 
            | CmsColor.Cmyk _ , Icc.Cmyk _ 
            | CmsColor.Gray _, Icc.Gray _ 
            | CmsColor.Lab _, Icc.Lab _
            | CmsColor.Rgb _, Icc.Rgb _ -> true
            | _ -> false

        member private x.GetConvertedColorValuesAsync(outputIcc: Icc, intent: Intent, inputIcc: Icc) = async {
            if x.IsSameColorSpaceWith(inputIcc)
            then
                let inputValues = x.GetColorValues()

                let msg = (ServerMsg.CalcColor (inputIcc, inputValues, outputIcc, intent))

                return
                    msgCache.GetOrAdd(msg, fun msg ->
                        GetClient() <? msg
                        |> Async.RunSynchronously
                    )

            else return x.GetColorValues()
        }

        member x.ConvertToAsync(outputIcc: Icc, ?intent: Intent, ?inputIcc: CmsColor -> Icc) = async {
            let inputIcc = (defaultArg inputIcc CmsColor.DefaultIcc) x
            let intent = defaultArg intent defaultIntent

            if inputIcc = outputIcc 
            then return x
            else
                let! (outputValues : float32[])  = x.GetConvertedColorValuesAsync(outputIcc, intent, inputIcc)

                match outputIcc with 
                | Icc.Lab _ ->
                    return CmsColor.Lab( {L = outputValues.[0]; a = outputValues.[1]; b = outputValues.[2]} )

                | Icc.Cmyk _ -> return CmsColor.Cmyk(new DeviceCmyk(outputValues.[0], outputValues.[1], outputValues.[2], outputValues.[3]))
                | Icc.Gray _ -> return CmsColor.Gray(new DeviceGray(Array.exactlyOne outputValues))
                | Icc.Rgb _ -> return CmsColor.Rgb(new DeviceRgb(outputValues.[0], outputValues.[1], outputValues.[2]))

        }

        member x.ConvertToLabAsync(?labIcc: LabIcc, ?intent: Intent, ?inputIcc: CmsColor -> Icc) = async {
            let labIcc = defaultArg labIcc defaultLabIcc
            let inputIcc = (defaultArg inputIcc CmsColor.DefaultIcc) x
            let intent = defaultArg intent defaultIntent

            if x.IsSameColorSpaceWith(inputIcc) 
            then 
                if inputIcc = Icc.Lab labIcc
                then
                    return x.AsLab().Value
                else
                    let! (outputValues : float32[])  = x.GetConvertedColorValuesAsync(Icc.Lab labIcc, intent, inputIcc)
                    return{L = outputValues.[0]; a = outputValues.[1]; b = outputValues.[2]}

            else 
                return failwithf "input icc %A and color %A are not in same namespace" inputIcc x
        }

        static member OfColor(color: Color) =
            match color with 
            | :? DeviceCmyk as cmyk -> CmsColor.Cmyk cmyk
            | :? DeviceRgb as rgb -> CmsColor.Rgb rgb
            | :? DeviceGray as gray -> CmsColor.Gray gray
            | :? Separation as separation -> CmsColor.Separation separation
            | _ -> failwithf "Color %A is not supported to convert to cms Color" color
            
    type Color with

        static member private Is(predicate, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let predicate =
                match predicateCompose with 
                | Some predicateCompose -> predicate >> predicateCompose 
                | None -> predicate

            fun (color: Color) ->
                let labColor = 
                    CmsColor.OfColor(color).ConvertToLabAsync(?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)
                    |> Async.RunSynchronously

                predicate labColor

        static member IsCmsGray(?maxDeviation: float32, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let maxDeviation = defaultArg maxDeviation defaultMaxCmsGrayDeviation
            let predicate (labColor: FsLab) = 
                abs labColor.a <= maxDeviation && abs labColor.b <= maxDeviation

            Color.Is(predicate, ?predicateCompose = predicateCompose, ?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)
                
        static member IsWhite(?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let predicate (labColor: FsLab) = 
                labColor.L = 100.f && labColor.a = 0.f && labColor.b = 0.f

            Color.Is(predicate, ?predicateCompose = predicateCompose, ?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)


    type Info = 
        static member ColorIsWhite(fillOrStrokeOptions, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            Info.ColorIs(fillOrStrokeOptions, Color.IsWhite(?labIcc = labIcc, ?predicateCompose = predicateCompose, ?intent = intent, ?inputIcc = inputIcc))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

        static member ColorIsCmsGray(fillOrStrokeOptions, ?predicateCompose, ?maxDeviation: float32, ?labIcc, ?intent, ?inputIcc) =
            Info.ColorIs(fillOrStrokeOptions, Color.IsCmsGray(?predicateCompose = predicateCompose, ?maxDeviation = maxDeviation ,?labIcc = labIcc,?intent = intent, ?inputIcc = inputIcc))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)



    type Modifier with
        static member BlackOrWhite(?fillOrStrokeModifyingOptions, ?labIcc, ?intent, ?inputIcc) =
            fun (args: _SelectionModifierFixmentArguments<'userState>) ->
                Modifier.ReplaceColor(
                    ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                    picker = 
                        fun color ->
                            if Color.IsWhite(predicateCompose = not,?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc) color
                            then Some (DeviceGray.BLACK :> Color)
                            else None
                ) args
        
        
        static member ReplaceColor(labPicker: FsLab -> Color option, ?fillOrStrokeModifyingOptions, ?labIcc, ?intent, ?inputIcc: CmsColor -> Icc) =
            Modifier.ReplaceColor(
                ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                picker = 
                    fun (color: Color) ->
                        match inputIcc with 
                        | Some inputIcc ->
                            let cmsColor = (CmsColor.OfColor color)
                            if cmsColor.IsSameColorSpaceWith(inputIcc cmsColor) then
                                cmsColor.ConvertToLabAsync(?labIcc = labIcc, ?intent = intent, inputIcc = inputIcc)
                                |> Async.RunSynchronously
                                |> labPicker
                            else 
                                None

                        | None -> 
                            (CmsColor.OfColor color).ConvertToLabAsync(?labIcc = labIcc, ?intent = intent)
                            |> Async.RunSynchronously
                            |> labPicker
                )


        static member ConvertColorsTo(outputIcc: Icc, ?fillOrStrokeModifyingOptions, ?predicate: Color -> bool, ?intent, ?inputIcc) =
            let predicate = defaultArg predicate (fun _ -> true)
            Modifier.ReplaceColor(
                ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                picker =
                    fun originColor -> 
                    if predicate originColor 
                    then 
                        let newColor = 
                            let cmsColor = (CmsColor.OfColor originColor)
                            cmsColor.ConvertToAsync(outputIcc, ?intent = intent, ?inputIcc = inputIcc)
                            |> Async.RunSynchronously
                        Some (newColor.GetColor())
                    else None
            )



    type InputIccModifyArgs(factory: (CmsColor -> Icc), ?inputIccName: string) =
        member val InputIccName = defaultArg inputIccName ""
        member x.Factory = factory

        member x.Info_IsColorInColorSpace(fillOrStrokeOptions: FillOrStrokeOptions, predicate) =
            Info.ColorIs(
                fillOrStrokeOptions,
                fun color ->
                    let cmsColor = CmsColor.OfColor color
                    let inputIcc = x.Factory cmsColor
                    cmsColor.IsSameColorSpaceWith(inputIcc)
                    && predicate color
            )
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

           

    type Modify =
        static member ConvertColorsTo(pageSelector: PageSelector, outputIcc: Icc, ?predicate, ?fillOrStrokeOptions: FillOrStrokeOptions, ?intent, ?inputIcc : InputIccModifyArgs) =
            let fillOrStrokeOptions = defaultArg fillOrStrokeOptions FillOrStrokeOptions.FillOrStroke
            let predicate = defaultArg predicate (fun _ -> true)

            let fillOrStrokeModifyingOptions =
                match fillOrStrokeOptions with 
                | FillOrStrokeOptions.FillAndStroke 
                | FillOrStrokeOptions.FillOrStroke -> FillOrStrokeModifingOptions.FillAndStroke
                | FillOrStrokeOptions.Fill -> FillOrStrokeModifingOptions.Fill
                | FillOrStrokeOptions.Stroke -> FillOrStrokeModifingOptions.Stroke

            modify(
                pageSelector,
                [
                    { Name =
                        match inputIcc with 
                        | Some inputIcc ->
                            sprintf "Convert all %A %s colors to outputIcc %A" fillOrStrokeOptions inputIcc.InputIccName outputIcc
                        | None -> sprintf "Convert %A all colors to outputIcc %A" fillOrStrokeOptions outputIcc
                      
                      Selector = 
                        PathOrText (
                            match inputIcc with 
                            | Some inputIcc -> inputIcc.Info_IsColorInColorSpace(fillOrStrokeOptions, predicate)
                            | None ->
                                Info.ColorIs(
                                    fillOrStrokeOptions,
                                    predicate
                                )
                        )
             
                      Modifiers = 
                        let inputIcc = 
                            inputIcc |> Option.map (fun m -> m.Factory)

                        [ Modifier.ConvertColorsTo(
                            fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                            outputIcc = outputIcc,
                            predicate = predicate,
                            ?intent = intent,
                            ?inputIcc = inputIcc
                        )]
                    }
                ]
            )

        static member ConvertColorsToDeviceGray(pageSelector: PageSelector, ?fillOrStrokeOptions, ?maxDeviation: float32, ?predicate, ?outputIcc: GrayIcc, ?labIcc, ?intent, ?inputIcc: InputIccModifyArgs) =
            let maxDeviation = defaultArg maxDeviation 0.f
            let outputIcc = 
                defaultArg outputIcc defaultGrayIcc |> Icc.Gray

            let predicate = defaultArg predicate (fun _ -> true)
            let inputIccFactory = 
                inputIcc |> Option.map (fun m -> m.Factory)

            Modify.ConvertColorsTo(
                pageSelector,
                outputIcc,
                ?fillOrStrokeOptions = fillOrStrokeOptions,
                ?intent = intent,
                ?inputIcc = inputIcc,
                predicate = 
                    fun color ->
                        predicate color 
                        && Color.IsCmsGray(maxDeviation = maxDeviation, ?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIccFactory) color
            )

  
