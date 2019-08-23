namespace Shrimp.Pdf
open Fake.IO
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.IO


module FileOperations =

    [<RequireQualifiedAccess>]
    type FileOperationOutputDirectoryOptions =
        | ReaderDirectoryPath
        | CustomDirectoryPath of string

    type DocumentSplitArguments =
        { PageNumPerDocument: int 
          OutputDirectory: FileOperationOutputDirectoryOptions
          Override: bool }
    with 
        static member DefalutValue =
            { PageNumPerDocument = 1 
              OutputDirectory = FileOperationOutputDirectoryOptions.ReaderDirectoryPath 
              Override =  true }


    let splitDocumentToMany (f: DocumentSplitArguments -> DocumentSplitArguments)  =
        let args = f DocumentSplitArguments.DefalutValue
        if args.PageNumPerDocument <= 0 then failwithf "Page num %d per document must be bigger than 1" args.PageNumPerDocument
        fun (flowModel) (readerDocument: ReaderDocument) ->
            let reader = readerDocument.Reader

            let outputDirectory =
                match args.OutputDirectory with 
                | FileOperationOutputDirectoryOptions.ReaderDirectoryPath -> 
                    Path.getDirectory flowModel.File

                | FileOperationOutputDirectoryOptions.CustomDirectoryPath directory ->
                    let directoryFullName = Path.GetFullPath directory
                    Directory.ensure directoryFullName
                    directoryFullName

            let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(flowModel.File)
            let newModels = 
                reader
                |> PdfDocument.getPages
                |> List.chunkBySize(args.PageNumPerDocument)
                |> List.mapi(fun i pages ->
                    let number = i + 1
                    let fileName = sprintf "%s_%d.pdf" fileNameWithoutExtension number
                    let fileFullPath = Path.Combine(outputDirectory, fileName)
                    match File.exists fileFullPath, args.Override with 
                    | true, false -> failwithf "File %s already exists" fileFullPath
                    | false, _ -> ()
                    | true, true ->
                        File.delete fileFullPath

                    let writer = new PdfDocument(new PdfWriter(fileFullPath))
                    for page in pages do 
                        let writerPageResource = page.CopyTo(writer)
                        writer.AddPage(writerPageResource) |> ignore
                    writer.Close()
                    { File = fileFullPath; UserState = flowModel.UserState }
                )
            let newFiles = 
                newModels
                |> List.map (fun m -> 
                    m.File
                )
                |> String.concat "\n"
            Logger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
            newModels

        |> FileOperation

