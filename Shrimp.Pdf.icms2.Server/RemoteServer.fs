module Shrimp.Pdf.icms2.Server.RemoteServer
open Shrimp.Akkling.Cluster.Intergraction
open Akkling

open Shrimp.Pdf.icms2.Model
open icms2_wrapper
open Fake.IO.FileSystemOperators

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
        let rec loop (model: Map<Icc * Icc * Indent, Icms>) = actor {
            let! msg = ctx.Receive()
            match msg with
            | ServerMsg.CalcColor (inputIcc, inputValues, outputIcc, indent) ->
                let indent = enum<Indent> (int indent)
                match Map.tryFind (inputIcc, outputIcc, indent) model with 
                | Some icms2 -> 
                    ctx.RespondSafely(fun _ ->
                        let outputValues = icms2.DoTransfrom(inputValues)
                        logger.Info (sprintf "[SHRIMP SERVER Pdf ICMS2] Convert %O %A to %O %A" inputIcc inputValues outputIcc outputValues) 
                        box outputValues
                    )

                | None ->
                    ctx.RespondSafely(fun _ ->
                        let icms2 = new Icms("Icc" </> Icc.fileName inputIcc, Icc.format inputIcc,"Icc" </> Icc.fileName outputIcc, Icc.format outputIcc, indent, 0u)
                        let outputValues = icms2.DoTransfrom(inputValues)
                        logger.Info (sprintf "[SHRIMP SERVER Pdf ICMS2] Convert %O %A to %O %A" inputIcc inputValues outputIcc outputValues) 
                        box outputValues 
                    )
        }
        loop Map.empty
    )
    |> ignore



