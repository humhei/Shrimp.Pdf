namespace Shrimp.Pdf.icms2.client
#nowarn "0104"
open icms2_wrapper

open Shrimp.Pdf.icms2

open Shrimp.Pdf.icms2.Core
open Shrimp.Pdf.icms2.CmsCore
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
open Shrimp.FSharp.Plus
open System.Collections.Concurrent
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Pdf.Colors

[<AutoOpen>]
module Client =

    [<RequireQualifiedAccess>]
    module private ColorSpace =
        let createWhite (colorSpace: ColorSpace) =
            match colorSpace with 
            | ColorSpace.Gray -> FsGray.WHITE           |> FsValueColor.Gray
            | ColorSpace.Cmyk -> FsDeviceCmyk.WHITE     |> FsValueColor.Cmyk
            | ColorSpace.Lab -> FsLab.WHITE             |> FsValueColor.Lab
            | ColorSpace.Rgb -> FsDeviceRgb.WHITE       |> FsValueColor.Rgb

        let createBlack (colorSpace: ColorSpace) =
            match colorSpace with 
            | ColorSpace.Gray -> FsGray.BLACK           |> FsValueColor.Gray
            | ColorSpace.Cmyk -> FsDeviceCmyk.BLACK     |> FsValueColor.Cmyk
            | ColorSpace.Lab -> FsLab.BLACK             |> FsValueColor.Lab
            | ColorSpace.Rgb -> FsDeviceRgb.BLACK       |> FsValueColor.Rgb


    type private AssemblyFinder = AssemblyFinder

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

    let defaultIntent: Indent =
        getEnumFromConfig "shrimp.pdf.icms2.client.icc.intent"

    let defaultMaxCmsGrayDeviation: float32 =
        referenceConfig.GetFloat("shrimp.pdf.icms2.client.maxCmsGrayDeviation")

    //[<AutoOpen>]
    //module ClientCluster =
        //let private client = 
        //    lazy
        //        let actorClient = Client.create()
        //        actorClient.WarmUp(fun _ ->
        //            actorClient <! ServerMsg.CalcColor (Icc.Rgb defaultRgbIcc, [|0.5f; 0.5f; 0.5f|], Icc.Cmyk defaultCmykIcc, defaultIntent)
        //        )
        //        actorClient

        //let GetDefaultClusterIcm2Client() = 
        //    client.Value



    //let private msgCache = new ConcurrentDictionary<ServerMsg, float32[]>()


    type ColorSpace with 
        static member DefaultIcc(colorSpace) =
            match colorSpace with
            | ColorSpace.Rgb  -> Icc.Rgb defaultRgbIcc
            | ColorSpace.Cmyk  -> Icc.Cmyk defaultCmykIcc
            | ColorSpace.Gray  -> Icc.Gray defaultGrayIcc
            | ColorSpace.Lab  -> Icc.Lab defaultLabIcc

    type AlternativeFsColor 
    with 

        static member DefaultIcc (cmsColor: AlternativeFsColor) =
            match cmsColor with 
            | AlternativeFsColor.IccBased iccBased -> iccBased.Icc
            | _ -> ColorSpace.DefaultIcc cmsColor.AlterColor.ColorSpace

        member x.AsLab() =
            match x.AlterColor with 
            | FsValueColor.Lab lab -> Some lab
            | _ -> None

        member private x.GetColorValues() = x.AlterColor.GetColorArrayValues()
            
        member x.IsSameColorSpaceWith(icc: Icc) = x.AlterColor.ColorSpace = icc.ColorSpace

        member private x.GetConvertedColorValues(outputIcc: Icc, intent: Indent, inputIcc: Icc) = 
            if x.IsSameColorSpaceWith(inputIcc)
            then
                match inputIcc.ColorSpace = outputIcc.ColorSpace with 
                | true -> x.GetColorValues()
                | false ->
                    let inputValues = x.GetColorValues()
                    (FsIcmsTransformer.CalcColor(inputIcc, inputValues, outputIcc, intent))
              

            else x.GetColorValues()

        member x.ConvertTo(outputIcc: Icc, ?intent: Indent, ?inputIcc: AlternativeFsColor -> Icc) = 
            
            let inputIcc = (defaultArg inputIcc AlternativeFsColor.DefaultIcc) x
            match inputIcc.ColorSpace = outputIcc.ColorSpace with 
            | true -> x.AlterColor
            | false ->
                if x.IsWhite()
                then (ColorSpace.createWhite outputIcc.ColorSpace)
                elif x.IsBlack()
                then (ColorSpace.createBlack outputIcc.ColorSpace)
                else
                    let intent = defaultArg intent defaultIntent

                    if inputIcc = outputIcc 
                    then x.AlterColor
                    else
                        let (outputValues : float32[])  = x.GetConvertedColorValues(outputIcc, intent, inputIcc)

                        match outputIcc with 
                        | Icc.Lab _ ->
                            FsValueColor.Lab( {L = outputValues.[0]; a = outputValues.[1]; b = outputValues.[2]} )

                        | Icc.Cmyk _ -> FsValueColor.Cmyk(FsDeviceCmyk.Create(outputValues.[0], outputValues.[1], outputValues.[2], outputValues.[3]))
                        | Icc.Gray _ -> FsValueColor.Gray(FsGray(outputValues.[0]))
                        | Icc.Rgb _ -> FsValueColor.Rgb(FsDeviceRgb.Create(outputValues.[0], outputValues.[1], outputValues.[2]))

        member x.ConvertToLab(?labIcc: LabIcc, ?intent: Indent, ?inputIcc: AlternativeFsColor -> Icc) = 
            
            if x.IsWhite()
            then (FsLab.WHITE)
            elif x.IsBlack()
            then (FsLab.BLACK)
            else
                let labIcc = defaultArg labIcc defaultLabIcc
                let inputIcc = (defaultArg inputIcc AlternativeFsColor.DefaultIcc) x
                let intent = defaultArg intent defaultIntent

                if x.IsSameColorSpaceWith(inputIcc) 
                then 
                    if inputIcc = Icc.Lab labIcc
                    then
                        x.AsLab().Value
                    else
                        let (outputValues : float32[])  = x.GetConvertedColorValues(Icc.Lab labIcc, intent, inputIcc)
                        {L = outputValues.[0]; a = outputValues.[1]; b = outputValues.[2]}

                else 
                    failwithf "input icc %A and color %A are not in same namespace" inputIcc x
        


    type AlternativeFsColor with

        static member private Is(predicate, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let predicate =
                match predicateCompose with 
                | Some predicateCompose -> predicate >> predicateCompose 
                | None -> predicate

            fun (color: AlternativeFsColor) ->
                let labColor = 
                    color.ConvertToLab(?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)

                predicate labColor

        static member IsCmsGray(?maxDeviation: float32, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let maxDeviation = defaultArg maxDeviation defaultMaxCmsGrayDeviation
            let predicate (labColor: FsLab) = 
                abs labColor.a <= maxDeviation && abs labColor.b <= maxDeviation

            AlternativeFsColor.Is(predicate, ?predicateCompose = predicateCompose, ?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)
                
        static member IsCmsWhite(?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            let predicate (labColor: FsLab) = 
                labColor.L = 100.f && labColor.a = 0.f && labColor.b = 0.f

            AlternativeFsColor.Is(predicate, ?predicateCompose = predicateCompose, ?labIcc = labIcc, ?intent = intent, ?inputIcc = inputIcc)

        member x.Cms_AsSpecificOrWhite(specificColor, ?labIcc, ?intent, ?inputIcc) =
            if AlternativeFsColor.IsCmsWhite(?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc) x
            then (DeviceGray.WHITE :> Color)
            else specificColor

        member x.Cms_AsBlackOrWhite(?labIcc, ?intent, ?inputIcc) =
            x.Cms_AsSpecificOrWhite(DeviceGray.BLACK, ?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc)

        member x.Cms_AsBlackOrWhite_Inversed(?labIcc, ?intent, ?inputIcc) =
            if AlternativeFsColor.IsCmsWhite(?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc) x
            then (DeviceGray.BLACK :> Color)
            else (DeviceGray.WHITE :> Color)

        member x.Cms_AsBlackOrWhite_Inversed_FsColor(?labIcc, ?intent, ?inputIcc) =
            if AlternativeFsColor.IsCmsWhite(?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc) x
            then (FsColor.BLACK)
            else (FsColor.WHITE)

    [<RequireQualifiedAccess>]
    module private FsColor =
        let predicateByAlternativeFsColor predicate (fsColor: FsColor) =
            match fsColor.AsAlternativeFsColor with 
            | Some v -> predicate v
            | None -> false


    type Info = 
        static member ColorIsCmsWhite(fillOrStrokeOptions, ?predicateCompose, ?labIcc, ?intent, ?inputIcc) =
            Info.ColorIs(fillOrStrokeOptions, FsColor.predicateByAlternativeFsColor <| AlternativeFsColor.IsCmsWhite(?labIcc = labIcc, ?predicateCompose = predicateCompose, ?intent = intent, ?inputIcc = inputIcc))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

        static member ColorIsWhite(fillOrStrokeOptions) =
            Info.ColorIs(fillOrStrokeOptions, FsColor.predicateByAlternativeFsColor <| (fun m -> m.IsWhite()))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

        static member ColorIsNotWhite(fillOrStrokeOptions) =
            Info.ColorIs(fillOrStrokeOptions, FsColor.predicateByAlternativeFsColor <| (fun m -> not(m.IsWhite())))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

        static member ColorIsCmsGray(fillOrStrokeOptions, ?predicateCompose, ?maxDeviation: float32, ?labIcc, ?intent, ?inputIcc) =
            Info.ColorIs(fillOrStrokeOptions, FsColor.predicateByAlternativeFsColor <| AlternativeFsColor.IsCmsGray(?predicateCompose = predicateCompose, ?maxDeviation = maxDeviation ,?labIcc = labIcc,?intent = intent, ?inputIcc = inputIcc))
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)

    


    type Modifier with
        static member SepcificOrWhite(specificColor, ?fillOrStrokeModifyingOptions, ?labIcc, ?intent, ?inputIcc) =
            fun (args: _SelectionModifierFixmentArguments<'userState>) ->
                Modifier.ReplaceColor(
                    ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                    picker = 
                        fun color ->
                            match color.AsAlternativeFsColor with 
                            | None -> None
                            | Some color ->
                                color.Cms_AsSpecificOrWhite(specificColor, ?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc)
                                |> Some
                ) args

        static member BlackOrWhite(?fillOrStrokeModifyingOptions, ?labIcc, ?intent, ?inputIcc) =
            fun (args: _SelectionModifierFixmentArguments<'userState>) ->
                Modifier.ReplaceColor(
                    ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                    picker = 
                        fun color ->
                            match color.AsAlternativeFsColor with 
                            | None -> None
                            | Some color ->
                                color.Cms_AsBlackOrWhite(?labIcc = labIcc,?intent = intent,?inputIcc = inputIcc)
                                |> Some
                ) args
        
        
        static member ReplaceColor(labPicker: FsLab -> Color option, ?fillOrStrokeModifyingOptions, ?labIcc, ?intent, ?inputIcc) =
            Modifier.ReplaceAlternativeColor(
                ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                picker = 
                    fun (cmsColor: AlternativeFsColor) ->
                        match inputIcc with 
                        | Some inputIcc ->
                            if cmsColor.IsSameColorSpaceWith(inputIcc cmsColor) then
                                cmsColor.ConvertToLab(?labIcc = labIcc, ?intent = intent, inputIcc = inputIcc)
                                |> labPicker
                            else 
                                None

                        | None -> 
                            cmsColor.ConvertToLab(?labIcc = labIcc, ?intent = intent)
                            |> labPicker
                )


        static member ConvertColorsTo(outputIcc: Icc, ?fillOrStrokeModifyingOptions, ?predicate: AlternativeFsColor -> bool, ?intent, ?inputIcc): Modifier<_> =
            let predicate = defaultArg predicate (fun _ -> true)
            fun args ->
                Modifier.ReplaceAlternativeColor(
                    ?fillOrStrokeModifyingOptions = fillOrStrokeModifyingOptions,
                    picker =
                        fun cmsColor -> 
                        if predicate cmsColor 
                        then 
                            let newColor = 
                                cmsColor.ConvertTo(outputIcc, ?intent = intent, ?inputIcc = inputIcc)

                            Some (FsValueColor.ToItextColor newColor)
                        else None
                ) args



    type InputIccModifyArgs(factory: (AlternativeFsColor -> Icc), ?inputIccName: string) =
        member val InputIccName = defaultArg inputIccName ""
        member x.Factory = factory

        member x.Info_IsColorInColorSpace(fillOrStrokeOptions: FillOrStrokeOptions, predicate) =
            Info.AlternativeColorIs(
                fillOrStrokeOptions,
                fun cmsColor ->
                    let inputIcc = x.Factory cmsColor
                    cmsColor.IsSameColorSpaceWith(inputIcc)
                    && predicate cmsColor
            )
            |> reSharp (fun (info: #IAbstractRenderInfo) -> info)



    type Modify_ConvertColorsTo_Options =
        { Predicate: AlternativeFsColor -> bool 
          Intent: Indent
          RenewableInfoTagPredicate: RenewableInfoTag -> bool
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
              Intent = defaultIntent
              RenewableInfoTagPredicate = fun _ -> true
              InputIccModifyArgs = None
              FillOrStrokeOptions = FillOrStrokeOptions.FillOrStroke
              PageInfosValidation = PageInfosValidation.ignore
              SelectorTag = SelectorTag.PathOrText
              Info_BoundIs_Args = None }

        member x.To_Modify_ReplaceColors_Options() : Modify_ReplaceColors_Options =
            { PageSelector = x.PageSelector 
              FillOrStrokeOptions = x.FillOrStrokeOptions 
              RenewableInfoTagPredicate = x.RenewableInfoTagPredicate
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


    type Modify_ConvertColorsToDeviceGray_Options =
        { MaxDeviation: float32 
          OutputIcc: GrayIcc 
          LabIcc: LabIcc 
          Modify_ConvertColorsTo_Options:  Modify_ConvertColorsTo_Options }

    with 
        static member DefaultValue =
            { MaxDeviation =  defaultMaxCmsGrayDeviation 
              OutputIcc = defaultGrayIcc 
              LabIcc = defaultLabIcc
              Modify_ConvertColorsTo_Options = Modify_ConvertColorsTo_Options.DefaultValue }

    type Modify with
        static member ConvertColorsTo(outputIcc: Icc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParameters: NameAndParameters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue

            let nameAndParameters = 
                { NameAndParameters.Name = "ConvertColorTo"
                  Parameters =
                    [
                        "outputIcc" => outputIcc.ToString()
                        "options" => options.ToString()
                    ]
                }
                |> defaultArg nameAndParameters

            Modify.ReplaceAlternativeColors(
                options = options.To_Modify_ReplaceColors_Options(),
                nameAndParameters = nameAndParameters,
                picker = 
                    fun cmsColor -> 
                        let inputIcc = 
                            options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)

                        if options.Predicate cmsColor 
                        then 
                            let newColor = 
                                cmsColor.ConvertTo(outputIcc, ?intent = Some options.Intent, ?inputIcc = inputIcc)

                            Some (FsValueColor.ToItextColor newColor)
                        else None
            )


        static member ConvertColorsToDeviceGray(?options: Modify_ConvertColorsToDeviceGray_Options, ?nameAndParameters: NameAndParameters) =
            let options = defaultArg options Modify_ConvertColorsToDeviceGray_Options.DefaultValue

            let nameAndParameters = 
                { NameAndParameters.Name = "ConvertColorToDeviceGray"
                  Parameters =
                    [
                        "options" => options.ToString()
                    ]
                }
                |> defaultArg nameAndParameters



            let modify_ConvertColorsTo_Options = options.Modify_ConvertColorsTo_Options

            Modify.ConvertColorsTo(
                Icc.Gray options.OutputIcc,
                nameAndParameters = nameAndParameters,
                options = 
                    { modify_ConvertColorsTo_Options with 
                        Predicate = 
                            fun color ->
                                let inputIccFactory = modify_ConvertColorsTo_Options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)
                                modify_ConvertColorsTo_Options.Predicate color 
                                && AlternativeFsColor.IsCmsGray(
                                    maxDeviation = options.MaxDeviation,
                                    labIcc = options.LabIcc,
                                    intent = modify_ConvertColorsTo_Options.Intent, ?inputIcc = inputIccFactory) color
                    }
                )         
                
        static member SpecificOrWhite(specificColor: PdfCanvasColor, ?labIcc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParameters: NameAndParameters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue
            let labIcc = defaultArg labIcc defaultLabIcc
            let nameAndParameters = 
                { NameAndParameters.Name = sprintf "Sepecific color %s Or White" specificColor.LoggingText
                  Parameters =
                    [
                        "options" => options.ToString()
                        "labIcc" => labIcc.ToString()
                    ]
                }
                |> defaultArg nameAndParameters

            Manipulate.Factory(fun _ doc ->
                let specificColor = lazy doc.Value.GetOrCreateColor(specificColor)
                Modify.ReplaceAlternativeColors(
                    options = options.To_Modify_ReplaceColors_Options(),
                    nameAndParameters = nameAndParameters,
                    picker = 
                        fun color ->
                            let inputIccFactory = options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)
                            if options.Predicate color 
                            then 
                                color.Cms_AsSpecificOrWhite(specificColor.Value, labIcc = labIcc, intent = options.Intent, ?inputIcc = inputIccFactory)
                                |> Some
                            else None
                )
            )


        static member BlackOrWhite(?labIcc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParameters: NameAndParameters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue
            let labIcc = defaultArg labIcc defaultLabIcc
            let nameAndParameters = 
                { NameAndParameters.Name = "BlackOrWhite"
                  Parameters =
                    [
                        "options" => options.ToString()
                        "labIcc" => labIcc.ToString()
                    ]
                }
                |> defaultArg nameAndParameters


            let inputIccFactory = options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)
            Modify.ReplaceAlternativeColors(
                options = options.To_Modify_ReplaceColors_Options(),
                nameAndParameters = nameAndParameters,
                picker = 
                    fun color ->
                        if options.Predicate color 
                        then 
                            color.Cms_AsBlackOrWhite(labIcc = labIcc, intent = options.Intent, ?inputIcc = inputIccFactory)
                            |> Some
                        else None
            )
                
    type Flows with
        static member BlackOrWhite_Negative_Film(?strokeWidthIncrement: StrokeWidthIncrement, ?labIcc, ?options: Modify_ConvertColorsTo_Options, ?nameAndParameters: NameAndParameters) =
            let options = defaultArg options Modify_ConvertColorsTo_Options.DefaultValue
            let labIcc = defaultArg labIcc defaultLabIcc
            let nameAndParameters = 
                { NameAndParameters.Name = "BlackOrWhite_Negative_Film"
                  Parameters =
                    [
                        "options" => options.ToString()
                        "labIcc" => labIcc.ToString()
                    ]
                }
                |> defaultArg nameAndParameters

            Flow.Reuse(
                Reuses.AddBackground(
                    PageBoxKind.ActualBox,
                    fun args -> 
                        { args with 
                            StrokeColor = NullablePdfCanvasColor.N
                            FillColor = 
                                NullablePdfCanvasColor.OfPdfCanvasColor
                                    (PdfCanvasColor.WHITE )}
                )
            )
            <+>
            Flow.Manipulate(
                Modify.ReplaceAlternativeColors(
                    options = options.To_Modify_ReplaceColors_Options(),
                    nameAndParameters = nameAndParameters,
                    picker = 
                        fun color ->
                            let inputIccFactory = options.InputIccModifyArgs |> Option.map (fun m -> m.Factory)

                            if options.Predicate color
                            then 
                                color.Cms_AsBlackOrWhite_Inversed(labIcc = labIcc, intent = options.Intent, ?inputIcc = inputIccFactory)
                                |> Some
                            else None
                )
                <+>

                (
                    match strokeWidthIncrement with 
                    | None -> Manipulate.dummy() ||>> ignore
                    | Some strokeWidthIncrement ->

                        Modify.ExpandStrokeWidth(
                            [FsColor.WHITE],
                            strokeWidthIncrement.Value,
                            PdfCanvasColor.WHITE,
                            lineJoinStyle = strokeWidthIncrement.LineJoinStyle
                        )
                )
            )






            

  
