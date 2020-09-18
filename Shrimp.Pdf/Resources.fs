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
open Shrimp.Akkling.Cluster.Intergraction.Configuration

//open Shrimp.Pdf.Parser

[<AutoOpen>]
module internal Config =

    type private AssemblyFinder = AssemblyFinder

    let internal config = 
        lazy
            ConfigurationFactory
                .FromResource<AssemblyFinder>("Shrimp.Pdf.reference.conf")
            |> Configuration.fallBackByApplicationConf

module Resources =
    open Fake.IO
    let internal resourceDirectory = 
        lazy
            Path.GetFullPath (config.Value.GetString("shrimp.pdf.resourcesDirectory"))


    [<RequireQualifiedAccess>]
    module PdfDocument =

        /// should storage local resources in disk first   
        /// then invoke it: e.g. obtainCmykMarkFromResource "CMYK" writer 
        let obtainMarkFromResources (fileNameWithoutExtension: string) (writer: PdfDocument) =

            let file = resourceDirectory.Value </> "Marks" </> fileNameWithoutExtension + ".pdf"
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
            let file = resourceDirectory.Value </> "Colors" </> fileNameWithoutExtension + ".pdf"
            lock writer (fun _ ->
                let doc = new PdfDocument(new PdfReader(file))

                let color = 
                    let parser = new NonInitialClippingPathPdfDocumentContentParser(doc)

                    let paths = 
                        NonInitialClippingPathPdfDocumentContentParser.parse 2 (RenderInfoSelector.Path (fun _ -> true)) parser
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



            
    
