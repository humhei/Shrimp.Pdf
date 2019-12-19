namespace Shrimp.Pdf.icms2
open Shrimp.Pdf.icms2.Model
open iText.Kernel.Colors
open Colourful
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

    let private msgCache = new ConcurrentDictionary<ServerMsg, float32[]>()


    [<RequireQualifiedAccess>]
    type CmsColor =
        | Lab of LabColor
        | Cmyk of DeviceCmyk
        | Rgb of DeviceRgb
        | Gray of DeviceGray
        | Separation of Separation
    with 
        member x.DefaultIcc() =
            match x with 
            | CmsColor.Rgb _ -> Icc.Rgb defaultRgbIcc
            | CmsColor.Cmyk _ -> Icc.Cmyk defaultCmykIcc
            | CmsColor.Gray _ -> Icc.Gray defaultGrayIcc
            | CmsColor.Lab _ -> Icc.Lab defaultLabIcc
            | CmsColor.Separation separation -> failwith "Not implement"
          

        member x.GetColorValues() =
            match x with 
            | CmsColor.Lab lab -> 
                [|float32 lab.L; float32 lab.a; float32 lab.b|]
            | CmsColor.Cmyk cmyk -> cmyk.GetColorValue()
            | CmsColor.Gray gray -> gray.GetColorValue()
            | CmsColor.Rgb rgb -> rgb.GetColorValue()
            | CmsColor.Separation separation -> failwith "Not implement"

        member private x.GetConvertedColorValuesAsync(outputIcc: Icc, intent: Intent, inputIcc: Icc) = async {
            
            let inputValues = x.GetColorValues()

            let msg = (ServerMsg.CalcColor (inputIcc, inputValues, outputIcc, intent))

            return
                msgCache.GetOrAdd(msg, fun msg ->
                    GetClient() <? msg
                    |> Async.RunSynchronously
                )
        }

        member x.ConvertToAsync(outputIcc: Icc, ?intent: Intent, ?inputIcc: Icc) = async {
            let inputIcc = defaultArg inputIcc (x.DefaultIcc())
            let intent = defaultArg intent defaultIntent

            let! (outputValues : float32[])  = x.GetConvertedColorValuesAsync(outputIcc, intent, inputIcc)

            match outputIcc with 
            | Icc.Lab _ ->
                let labValues = new ReadOnlyCollection<float>(outputValues |> Array.map float)
                return CmsColor.Lab(new LabColor(labValues))

            | Icc.Cmyk _ -> return CmsColor.Cmyk(new DeviceCmyk(outputValues.[0], outputValues.[1], outputValues.[2], outputValues.[3]))
            | Icc.Gray _ -> return CmsColor.Gray(new DeviceGray(Array.exactlyOne outputValues))
            | Icc.Rgb _ -> return CmsColor.Rgb(new DeviceRgb(outputValues.[0], outputValues.[1], outputValues.[2]))

        }

        member x.ConvertToLabAsync(?labIcc: LabIcc, ?intent: Intent, ?inputIcc: Icc) = async {
            let labIcc = defaultArg labIcc defaultLabIcc
            let inputIcc = defaultArg inputIcc (x.DefaultIcc())
            let intent = defaultArg intent defaultIntent

            let! (outputValues : float32[])  = x.GetConvertedColorValuesAsync(Icc.Lab labIcc, intent, inputIcc)
            let labValues = new ReadOnlyCollection<float>(outputValues |> Array.map float)
            return new LabColor(labValues)
        }

        static member OfColor(color: Color) =
            match color with 
            | :? DeviceCmyk as cmyk -> CmsColor.Cmyk cmyk
            | :? DeviceRgb as rgb -> CmsColor.Rgb rgb
            | :? DeviceGray as gray -> CmsColor.Gray gray
            | :? Separation as separation -> CmsColor.Separation separation
            | _ -> failwithf "Color %A is not supported to convert to cms Color" color
            


    type Info = 
        static member StrokeColorIsWhite() =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) -> 
                IAbstractRenderInfo.hasStroke info 
                &&
                    let labColor = 
                        CmsColor.OfColor(info.Value.GetStrokeColor()).ConvertToLabAsync()
                        |> Async.RunSynchronously

                    labColor.L = 100. && labColor.a = 0. && labColor.b = 0.

        static member StrokeColorIsNotWhite() =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) -> 
                IAbstractRenderInfo.hasStroke info 
                && not (Info.StrokeColorIsWhite() args info)

        static member FillColorIsWhite() =

            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) -> 

                IAbstractRenderInfo.hasFill info 
                &&

                    let labColor = 
                        CmsColor.OfColor(info.Value.GetFillColor()).ConvertToLabAsync()
                        |> Async.RunSynchronously

                    labColor.L = 100. && labColor.a = 0. && labColor.b = 0.


        static member FillColorIsNotWhite() =
            fun (args: PageModifingArguments<_>) (info: #IAbstractRenderInfo) -> 
                IAbstractRenderInfo.hasFill info 
                && not (Info.FillColorIsWhite() args info)


    type Modify =
        static member BlackOrWhite() =
            fun (args: _SelectionModifierFixmentArguments<'userState>) ->
                match Info.FillColorIsNotWhite() args.PageModifingArguments args.CurrentRenderInfo,Info.StrokeColorIsNotWhite() args.PageModifingArguments args.CurrentRenderInfo with 
                | true, true -> Modify.SetFillAndStrokeColor(DeviceGray.BLACK) args
                | true, false -> Modify.SetFillColor(DeviceGray.BLACK) args
                | false, true -> Modify.SetStrokeColor(DeviceGray.BLACK) args
                | _ -> [PdfCanvas.writeOperatorRange args.Close]
        





