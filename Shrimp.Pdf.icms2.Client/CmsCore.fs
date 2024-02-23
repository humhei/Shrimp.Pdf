namespace Shrimp.Pdf.icms2
open Akkling
open Akka.Configuration
open Shrimp.Akkling.Cluster
open Shrimp.Akkling.Cluster.Intergraction
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open Shrimp.FSharp.Plus
open System.IO
open System.Drawing.Imaging
open System.Drawing
open icms2_wrapper
open Shrimp.Pdf.Resources
open Shrimp.Pdf


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


module CmsCore = 
    let private model = System.Collections.Concurrent.ConcurrentDictionary()
    let private iccDir = resourceDirectory </> "Icc"

    let private getOrAddIcms(key) =
        let (inputIcc, outputIcc, indent) = key
        model.GetOrAdd(key, valueFactory = fun key ->
            let icms2 = new Icms(iccDir </> Icc.fileName inputIcc, Icc.format inputIcc,iccDir </> Icc.fileName outputIcc, Icc.format outputIcc, indent, 0u)
            icms2
        )

    type FsIcmsTransformer =
        static member CalcColor(inputIcc, inputValues, outputIcc, indent: Indent) =
            let inputValues =
                match inputIcc with 
                | Icc.Cmyk _ -> inputValues |> Array.map(fun m -> m * 100.f)
                | _ -> inputValues

            let icms2 = getOrAddIcms(inputIcc, outputIcc, indent)
            let outputValues = icms2.DoTransfrom(inputValues)
            let outputValues =
                match outputIcc with 
                | Icc.Cmyk _ -> inputValues |> Array.map(fun m -> m / 100.f)
                | _ -> outputValues
     
            outputValues

        static member ConvertImageColorSpace (inputIcc, storage: IndexableBitmapColorValuesStorage, outputIcc, indent: Indent) =
            let icms2 = getOrAddIcms(inputIcc, outputIcc, indent)
            

            let writer = Path.ChangeExtension(storage.File.Path, ".writer.raw")

            let colorValueLists =
                let depth = 
                    match inputIcc with 
                    | Icc.Rgb _ -> 3
                    | Icc.Cmyk _ -> 4
                    | Icc.Gray _ -> 1
                    | Icc.Lab _ -> 3

                match storage with 
                | IndexableBitmapColorValuesStorage.Origin storage ->
                    let bitmap = BitmapColorValues.OfStorage (storage)

                    match inputIcc with 
                    | Icc.Rgb _ -> bitmap.GetAsRgbValues()
                    | Icc.Cmyk _ -> bitmap.GetAsCMYKValues()
                    | Icc.Gray _ -> bitmap.GetAsGrayValues()
                    | Icc.Lab _ -> failwith "[ConvertImageColorSpace] Not implemented when input icc is Lab"

                | IndexableBitmapColorValuesStorage.Express (size, rawFile)
                | IndexableBitmapColorValuesStorage.Indexed (size, rawFile) ->
                    let bytes = System.IO.File.ReadAllBytes rawFile.Path

                    let multiple =
                        match inputIcc with 
                        | Icc.Rgb _ -> 1.
                        | Icc.Cmyk _ -> 100.
                        | Icc.Gray _ -> 1.
                        | Icc.Lab _ -> failwith "Not implement"


                    bytes
                    |> Array.chunkBySize depth
                    |> Array.map (fun bytes -> 
                        bytes
                        |> Array.map (fun m -> float m / 255. * multiple |> float32 )
                    )


            
            let valuesCount = outputIcc.ColorValuesCount

            let newByteArray = 
                colorValueLists
                |> Array.collect(fun colorValues ->
                    
                    let inputValues = colorValues
                    let outputvalues = 
                        let isBlack(icc, inputValues) =
                            match icc, inputValues with 
                            | Icc.Gray _, [|0.f|]
                            | Icc.Rgb _ ,  [|0.f; 0.f; 0.f|]
                            | Icc.Cmyk _,  [|_; _; _; 100.f|]
                            | Icc.Lab _ , [|0.f; 0.f; 0.f|] -> true
                            | _ -> false

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
                        | inputValues when isBlack (inputIcc, inputValues) -> black(outputIcc)
                        | EqualTo (white(inputIcc)) -> white(outputIcc)
                        | _ ->
                            icms2.DoTransfrom(inputValues)
                            |> Array.take valuesCount

                    outputvalues
                    |> Array.map(fun outputValue -> byte (outputValue * 255.f))
                )
                
            File.WriteAllBytes(writer, newByteArray)

            PdfLogger.info (sprintf "[SHRIMP SERVER Pdf ICMS2] Convert %O %A to %O %A" inputIcc storage outputIcc writer) 
            writer
            |> RawFile
