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
        let mutable private client: option<Client<unit, ServerMsg>> = None
        let SetClientContext() = 
            match client with 
            | None -> client <- Some (Client.create())
            | Some _ -> ()

        let GetClient() = 
            match client with
            | Some client -> client
            | None -> failwith "Please run SetClientContext() first to aceess icm server"


    let private referenceConfig = 
        lazy
            ConfigurationFactory.FromResource<AssemblyFinder>("Shrimp.Pdf.icms2.Client.reference.conf")
            |> Configuration.fallBackByApplicationConf

    let private getEnumFromConfig configName : 'enum =
        let avaliableIccs = Enum.GetNames(typeof<'enum>)
        let cmykIccText = referenceConfig.Value.GetString(configName)
        match Array.tryFind (fun m -> String.Compare(m, cmykIccText.Trim(), true) = 0) avaliableIccs with 
        | Some icc -> 
            Enum.Parse(typeof<'enum>, icc)
            |> unbox
        | None -> failwithf "avaliableIccs %A not include %s" avaliableIccs cmykIccText 
     


    let defaultCmykIcc: Lazy<CmykIcc> = 
        lazy
            getEnumFromConfig "shrimp.pdf.icms2.client.icc.cmyk"

    let defaultRgbIcc: Lazy<RgbIcc> =
        lazy
            getEnumFromConfig "shrimp.pdf.icms2.client.icc.rgb"

    let defaultLabIcc: Lazy<LabIcc> =
        lazy
            getEnumFromConfig "shrimp.pdf.icms2.client.icc.lab"

    let defaultGrayIcc: Lazy<GrayIcc> =
        lazy
            getEnumFromConfig "shrimp.pdf.icms2.client.icc.gray"

    let defaultIntent: Lazy<Intent> =
        lazy
            getEnumFromConfig "shrimp.pdf.icms2.client.icc.intent"

    let defaultMaxCmsGrayDeviation: Lazy<float32> =
        lazy
            referenceConfig.Value.GetFloat("shrimp.pdf.icms2.client.maxCmsGrayDeviation")

    let private msgCache = new ConcurrentDictionary<ServerMsg, float32[]>()


    [<RequireQualifiedAccess>]
    type CmsColor =
        | Lab of FsLab
        | Cmyk of DeviceCmyk
        | Rgb of DeviceRgb
        | Gray of DeviceGray
        | Separation of Separation
    with 
        member x.GetColorSpace() =
            match x with
            | CmsColor.Rgb _ -> ColorSpace.Rgb
            | CmsColor.Cmyk _ -> ColorSpace.Cmyk
            | CmsColor.Gray _ -> ColorSpace.Gray
            | CmsColor.Lab _ -> ColorSpace.Lab
            | CmsColor.Separation separation -> separation.GetFsColorSpace()

        static member DefaultIcc (cmsColor: CmsColor) =
            match cmsColor.GetColorSpace() with
            | ColorSpace.Rgb  -> Icc.Rgb defaultRgbIcc.Value
            | ColorSpace.Cmyk  -> Icc.Cmyk defaultCmykIcc.Value
            | ColorSpace.Gray  -> Icc.Gray defaultGrayIcc.Value
            | ColorSpace.Lab  -> Icc.Lab defaultLabIcc.Value
          
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
            | CmsColor.Separation separation -> separation.GetAlterateColor() |> FsValueColor.ToItextColor

        member x.GetColorValues() =
            match x with 
            | CmsColor.Lab lab -> 
                [|lab.L; lab.a; lab.b|]
            | CmsColor.Cmyk cmyk -> cmyk.GetColorValue()
            | CmsColor.Gray gray -> gray.GetColorValue()
            | CmsColor.Rgb rgb -> rgb.GetColorValue()
            | CmsColor.Separation separation -> separation.GetAlterateColor().GetColorValue() |> List.toArray

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
            let intent = defaultArg intent defaultIntent.Value

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
            let labIcc = defaultArg labIcc defaultLabIcc.Value
            let inputIcc = (defaultArg inputIcc CmsColor.DefaultIcc) x
            let intent = defaultArg intent defaultIntent.Value

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
            | :? Lab as lab -> 
                let colorValue = lab.GetColorValue()
                { L = colorValue.[0]
                  a = colorValue.[1]
                  b = colorValue.[2]
                } |> CmsColor.Lab
            | _ -> failwithf "Color %A is not supported to convert to cms Color" color
        
    [<RequireQualifiedAccess>]
    module CmsColor =
        let asLab = function
            | CmsColor.Lab color -> Some color
            | _ -> None

        let asCmyk = function
            | CmsColor.Cmyk color -> Some color
            | _ -> None

        let asRgb = function
            | CmsColor.Rgb color -> Some color
            | _ -> None


        let asGray = function
            | CmsColor.Gray color -> Some color
            | _ -> None

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
            let maxDeviation = defaultArg maxDeviation defaultMaxCmsGrayDeviation.Value
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



    type Modify_ConvertColorsTo_Options =
        { Predicate: Color -> bool 
          Intent: Intent
          InputIccModifyArgs: InputIccModifyArgs option
          PageSelector: PageSelector 
          FillOrStrokeOptions: FillOrStrokeOptions 
          PageInfosValidation: PageInfosValidation
          SelectorTag: SelectorTag
          Info_BoundIs_Args: Info_BoundIs_Args option }

    with 
        static member DefaultValue =
            { PageSelector = PageSelector.All 
              Predicate = fun _ -> true 
              Intent = defaultIntent.Value 
              InputIccModifyArgs = None
              FillOrStrokeOptions = FillOrStrokeOptions.FillOrStroke
              PageInfosValidation = PageInfosValidation.ignore
              SelectorTag = SelectorTag.PathOrText
              Info_BoundIs_Args = None }

        member x.To_Modify_ReplaceColors_Options() : Modify_ReplaceColors_Options =
            { PageSelector = x.PageSelector 
              FillOrStrokeOptions = x.FillOrStrokeOptions 
              PageInfosValidation = x.PageInfosValidation 
              Info_BoundIs_Args = x.Info_BoundIs_Args
              SelectorTag = x.SelectorTag }

        member x.Apply_Modify_ReplaceColors_Options(options: Modify_ReplaceColors_Options) =
            { x with 
                FillOrStrokeOptions = options.FillOrStrokeOptions 
                PageSelector = options.PageSelector  
                SelectorTag = options.SelectorTag 
                Info_BoundIs_Args = options.Info_BoundIs_Args 
                PageInfosValidation = options.PageInfosValidation }


    type Modfy_ConvertColorsToDeviceGray_Options =
        { MaxDeviation: float32 
          OutputIcc: GrayIcc 
          LabIcc: LabIcc 
          Modify_ConvertColorsTo_Options:  Modify_ConvertColorsTo_Options }

    with 
        static member DefaultValue =
            { MaxDeviation =  defaultMaxCmsGrayDeviation.Value 
              OutputIcc = defaultGrayIcc.Value 
              LabIcc = defaultLabIcc.Value
              Modify_ConvertColorsTo_Options = Modify_ConvertColorsTo_Options.DefaultValue }

    type Modify with

        static member ConvertColorsTo(outputIcc: Icc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParamters: NameAndParamters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue

            let nameAndParamters = 
                { Name = "ConvertColorTo"
                  Parameters =
                    [
                        "outputIcc" => outputIcc.ToString()
                        "options" => options.ToString()
                    ]
                }
                |> defaultArg nameAndParamters

            Modify.ReplaceColors(
                options = options.To_Modify_ReplaceColors_Options(),
                nameAndParamters = nameAndParamters,
                picker = 
                    fun color -> 
                        let inputIcc = 
                            options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)

                        if options.Predicate color 
                        then 
                            let newColor = 
                                let cmsColor = (CmsColor.OfColor color)
                                cmsColor.ConvertToAsync(outputIcc, ?intent = Some options.Intent, ?inputIcc = inputIcc)
                                |> Async.RunSynchronously
                            Some (newColor.GetColor())
                        else None
            )


        static member ConvertColorsToDeviceGray(?options: Modfy_ConvertColorsToDeviceGray_Options, ?nameAndParamters: NameAndParamters) =
            let options = defaultArg options Modfy_ConvertColorsToDeviceGray_Options.DefaultValue

            let nameAndParamters = 
                { Name = "ConvertColorToEeviceGray"
                  Parameters =
                    [
                        "options" => options.ToString()
                    ]
                }
                |> defaultArg nameAndParamters

            let modify_ConvertColorsTo_Options = options.Modify_ConvertColorsTo_Options

            Modify.ConvertColorsTo(
                Icc.Gray options.OutputIcc,
                nameAndParamters = nameAndParamters,
                options = 
                    { modify_ConvertColorsTo_Options with 
                        Predicate = 
                            fun color ->
                                let inputIccFactory = modify_ConvertColorsTo_Options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)
                                modify_ConvertColorsTo_Options.Predicate color 
                                && Color.IsCmsGray(
                                    maxDeviation = options.MaxDeviation,
                                    labIcc = options.LabIcc,
                                    intent = modify_ConvertColorsTo_Options.Intent, ?inputIcc = inputIccFactory) color
                    }
                )         
                
        static member BlackOrWhite(?labIcc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParamters: NameAndParamters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue
            let labIcc = defaultArg labIcc defaultLabIcc.Value
            let nameAndParamters = 
                { Name = "BlackOrWhite"
                  Parameters =
                    [
                        "options" => options.ToString()
                        "labIcc" => labIcc.ToString()
                    ]
                }
                |> defaultArg nameAndParamters


            Modify.ReplaceColors(
                options = options.To_Modify_ReplaceColors_Options(),
                nameAndParamters = nameAndParamters,
                picker = 
                    fun color ->
                        let inputIccFactory = options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)
                        if options.Predicate color && Color.IsWhite(predicateCompose = not,labIcc = labIcc, intent = options.Intent, ?inputIcc = inputIccFactory) color
                        then Some (DeviceGray.BLACK :> Color)
                        else None
            )
                





            

  
