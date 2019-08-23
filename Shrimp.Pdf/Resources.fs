namespace Shrimp.Pdf

open iText.Kernel.Pdf
open System.Reflection
open System.IO
open System.Text
open Akka.Configuration
open Fake.IO.FileSystemOperators
open iText.Kernel.Pdf.Canvas.Parser
open Shrimp.Pdf.Parser
open iText.Kernel.Colors
open iText.Kernel.Pdf.Colorspace


//open Shrimp.Pdf.Parser

[<AutoOpen>]
module internal Config =
    let private possibleFolders = 
        [ "../Assets"(*UWP*) ] 

    type private AssemblyFinder = AssemblyFinder

    /// application.conf should be copied to target folder
    let private fallBackByApplicationConf config =
        let folder = System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let folders = 
            [ folder ] 
            @ possibleFolders
                |> List.map (fun m -> Path.Combine(folder, m))

        folders 
        |> List.map (fun folder -> Path.Combine(folder, "application.conf"))
        |> List.tryFind (fun file -> File.Exists(file))
        |> function
            | Some file ->
                let texts = File.ReadAllText(file, Encoding.UTF8)
                let applicationConfig = ConfigurationFactory.ParseString(texts)
                applicationConfig.WithFallback(config)
            | None -> config

    let internal config = 
        ConfigurationFactory
            .FromResource<AssemblyFinder>("Shrimp.Pdf.reference.conf")
        |> fallBackByApplicationConf

[<AutoOpen>]
module Resources =
    open Fake.IO
    let internal resourceDirectory = 
        Path.GetFullPath (config.GetString("shrimp.pdf.resourcesDirectory"))


    [<RequireQualifiedAccess>]
    module PdfDocument =

        /// should storage local resources in disk first   
        /// then invoke it: e.g. obtainCmykMarkFromResource "CMYK" writer 
        let obtainMarkFromResources (fileNameWithoutExtension: string) (writer: PdfDocument) =

            let file = resourceDirectory </> "Marks" </> fileNameWithoutExtension + ".pdf"
            lock writer (fun _ ->
                let doc = new PdfDocument(new PdfReader(file))
                let markPage = 
                    doc.GetFirstPage()
                
                let formXObject = markPage.CopyAsFormXObject(writer)
                doc.Close()
                formXObject
            )

        /// should storage local resources in disk first   
        /// then invoke it: e.g. obtainColorFromResource "registration" writer 
        /// e.g. obtainColorFromResource @"Pantone+ Solid Coated/PANTONE 100 C" writer 
        let obtainSperationColorFromResources (fileNameWithoutExtension: string) (writer: PdfDocument) =
            let file = resourceDirectory </> "Colors" </> fileNameWithoutExtension + ".pdf"
            lock writer (fun _ ->
                let doc = new PdfDocument(new PdfReader(file))

                let color = 
                    let parser = new PdfDocumentContentParser(doc)

                    let paths = 
                        PdfDocumentContentParser.parse 2 (RenderInfoSelector.Path (fun _ -> true)) parser
                        |> Seq.choose IIntegratedRenderInfo.asIPathRenderInfo
                    let path = paths |> Seq.exactlyOne

                    path.PathRenderInfo.GetFillColor()

                match color with 
                | :? Separation -> 

                    let colorSpace = 
                        PdfColorSpace.MakeColorSpace(color.GetColorSpace().GetPdfObject().CopyTo(writer))

                    let injectedColor = Color.MakeColor(colorSpace, color.GetColorValue())
                    
                    doc.Close()

                    injectedColor

                | _ -> 
                    doc.Close()
                    failwithf "color %O is not a speration color" color
            )



            
    
