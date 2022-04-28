module Shrimp.Pdf.icms2.Server.RemoteServer
open Shrimp.Akkling.Cluster.Intergraction
open Akkling
open Shrimp.Pdf.icms2
open icms2_wrapper
open Fake.IO.FileSystemOperators
open System.Drawing
open System.Collections.Generic
open System.IO
open Shrimp.FSharp.Plus

[<RequireQualifiedAccess>]
module Icc =
    let fileName = function
        | Icc.Cmyk cmyk ->
            match cmyk with 
            | CmykIcc.JapanColor2001Coated -> "JapanColor2001Coated.ICC"
            | CmykIcc.USWebCoatedSWOP -> "USWebCoatedSWOP.ICC"
            | _ -> failwith "Not implemented"
        | Icc.Gray gray ->
            match gray with 
            | GrayIcc.``Dot Gain 15%`` -> "Dot Gain 15%.icc"
            | GrayIcc.``Dot Gain 20%`` -> "Dot Gain 20%.icc"
            | GrayIcc.``Dot Gain 25%`` -> "Dot Gain 25%.icc"
            | GrayIcc.``Dot Gain 30%`` -> "Dot Gain 30%.icc"
            | _ -> failwith "Not implemented"
        | Icc.Lab lab -> 
            match lab with 
            | LabIcc.``CIE Lab`` -> "CIE Lab.icc"
            | _ -> failwith "Not implemented"
        | Icc.Rgb rgb ->
            match rgb with 
            | RgbIcc.AdobeRGB1998 -> "AdobeRGB1998.icc"
            | RgbIcc.AppleRGB -> "AppleRgb.icc"
            | RgbIcc.``SRGB Color Space Profile`` -> "sRGB Color Space Profile.icm"
            | _ -> failwith "Not implemented"

    let format = function
        | Icc.Cmyk cmyk -> Format.TYPE_CMYK_FLT
        | Icc.Gray gray -> Format.TYPE_GRAY_FLT
        | Icc.Lab lab -> Format.TYPE_Lab_FLT
        | Icc.Rgb rgb -> Format.TYPE_RGB_FLT



let run() =
    Server.createAgent (fun ctx ->
        let logger = ctx.Log.Value
        let rec loop (model: Dictionary<Icc * Icc * Indent, Icms>) = actor {
            let! msg = ctx.Receive()
            let getOrAddIcms(key) =
                let (inputIcc, outputIcc, indent) = key
                match model.TryGetValue key with 
                | true, v -> v
                | false, _ ->
                    let icms2 = new Icms("Icc" </> Icc.fileName inputIcc, Icc.format inputIcc,"Icc" </> Icc.fileName outputIcc, Icc.format outputIcc, indent, 0u)
                    model.Add(key, icms2)
                    icms2
            
            match msg with
            | ServerMsg.CalcColor (inputIcc, inputValues, outputIcc, indent) ->
                let inputValues =
                    match inputIcc with 
                    | Icc.Cmyk _ -> inputValues |> Array.map(fun m -> m * 100.f)
                    | _ -> inputValues

                let indent = enum<Indent> (int indent)
                let icms2 = getOrAddIcms(inputIcc, outputIcc, indent)
                let outputValues = icms2.DoTransfrom(inputValues)
                let outputValues =
                    match outputIcc with 
                    | Icc.Cmyk _ -> inputValues |> Array.map(fun m -> m / 100.f)
                    | _ -> outputValues

                logger.Info (sprintf "[SHRIMP SERVER Pdf ICMS2] Convert %O %A to %O %A" inputIcc inputValues outputIcc outputValues) 

                ctx.Sender() <! outputValues


            | ServerMsg.ConvertImageColorSpace (inputIcc, bmpFile, outputIcc, indent) ->
                let indent = enum<Indent> (int indent)
                let icms2 = getOrAddIcms(inputIcc, outputIcc, indent)
                
                let bitmap = BitmapColorValues.OfRawFile bmpFile
                let writer = Path.ChangeExtension(bmpFile.Path, ".writer.raw")

                let colorValueLists =
                    match inputIcc with 
                    | Icc.Rgb _ -> bitmap.GetAsRgbValues()
                    | Icc.Cmyk _ -> bitmap.GetAsCMYKValues()
                    | Icc.Gray _ -> bitmap.GetAsGrayValues()
                    | Icc.Lab _ -> failwith "[ConvertImageColorSpace] Not implemented when input icc is Lab"

                
                let valuesCount = outputIcc.ColorValuesCount

                let newByteArray = 
                    colorValueLists
                    |> Array.collect(fun colorValues ->
                        
                        let inputValues = colorValues
                        let outputvalues = 
                            let black(icc) =
                                match icc with 
                                | Icc.Gray _ -> [|0.f|]
                                | Icc.Rgb _ -> [|0.f; 0.f; 0.f|]
                                | Icc.Cmyk _ -> [|0.f; 0.f; 0.f; 100.f|]
                                | Icc.Lab _ -> [|0.f; 0.f; 0.f|]

                            let white(icc) =
                                match icc with 
                                | Icc.Gray _ -> [|1.f|]
                                | Icc.Rgb _ -> [|1.f; 1.f; 1.f|]
                                | Icc.Cmyk _ -> [|0.f; 0.f; 0.f; 0.f|]
                                | Icc.Lab _ -> [|100.f; 0.f; 0.f|]

                            match inputValues with 
                            | EqualTo (black(inputIcc)) -> black(outputIcc)
                            | EqualTo (white(inputIcc)) -> white(outputIcc)
                            | _ ->
                                icms2.DoTransfrom(inputValues)
                                |> Array.take valuesCount

                        outputvalues
                        |> Array.map(fun outputValue -> byte (outputValue * 255.f))
                    )
                    
                File.WriteAllBytes(writer, newByteArray)

                logger.Info (sprintf "[SHRIMP SERVER Pdf ICMS2] Convert %O %A to %O %A" inputIcc bmpFile outputIcc writer) 
                ctx.Sender() <! RawFile writer



        }

        let calcColor (inputIcc, inputValues, outputIcc, indent) =
            let icms2 = new Icms("Icc" </> Icc.fileName inputIcc, Icc.format inputIcc,"Icc" </> Icc.fileName outputIcc, Icc.format outputIcc, indent, 0u)
            let outputValues = icms2.DoTransfrom(inputValues)
            outputValues 

        let msg1 =
            calcColor (Icc.Rgb RgbIcc.``SRGB Color Space Profile``, [|0.5f; 0.5f; 0.5f|], Icc.Cmyk CmykIcc.JapanColor2001Coated, Indent.INTENT_PERCEPTUAL)

        let msg2 =
            calcColor (Icc.Rgb RgbIcc.``SRGB Color Space Profile``, [|0.f; 0.f; 0.f|], Icc.Gray GrayIcc.``Dot Gain 15%``, Indent.INTENT_PERCEPTUAL)

        let msg3 =
            calcColor (Icc.Cmyk CmykIcc.JapanColor2001Coated, [|0.f; 100.f; 0.f; 0.f|], Icc.Gray GrayIcc.``Dot Gain 15%``, Indent.INTENT_PERCEPTUAL)

        let msg4 =
            calcColor (Icc.Rgb RgbIcc.``SRGB Color Space Profile``, [|1.f; 0.f; 0.f|], Icc.Lab LabIcc.``CIE Lab``, Indent.INTENT_PERCEPTUAL)

        let msg5 =
            calcColor (Icc.Rgb RgbIcc.``SRGB Color Space Profile``, [|1.f; 0.f; 0.f|], Icc.Cmyk CmykIcc.JapanColor2001Coated, Indent.INTENT_PERCEPTUAL)

        let msg6 =
            calcColor (Icc.Cmyk CmykIcc.JapanColor2001Coated, [|0.f; 100.f; 0.f; 0.f|], Icc.Lab LabIcc.``CIE Lab``, Indent.INTENT_PERCEPTUAL)


        let a = 1

        loop (new Dictionary<_, _>())


    )
    |> ignore



