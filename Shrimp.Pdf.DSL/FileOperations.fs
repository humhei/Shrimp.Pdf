namespace Shrimp.Pdf
open Fake.IO
open Shrimp.Pdf.Extensions
open iText.Kernel.Pdf
open System.IO

[<RequireQualifiedAccess>]
type DocumentSplitOutputDirectoryOptions =
    | ReaderDirectoryPath
    | CustomDirectoryPath of string

type DocumentSplitArguments =
    { ChunkSize: int 
      OutputDirectory: DocumentSplitOutputDirectoryOptions
      Override: bool }
with 
    static member DefalutValue =
        { ChunkSize = 1 
          OutputDirectory = DocumentSplitOutputDirectoryOptions.ReaderDirectoryPath 
          Override = false }
    
type DocumentMergingArguments =
    { TargetDocumentPath: string 
      Override: bool }
with 
    static member DefalutValue =
        { TargetDocumentPath = Path.GetTempFileName() |> Path.changeExtension ".pdf"
          Override = false }

[<RequireQualifiedAccess>]
module FileOperations =
    /// expose this function for modifyAsync
    let internal mergeDocumentsInternal targetDocumentName (writer: PdfDocument)  =
        fun (flowModels: FlowModel<'userState> list) ->
            for pageNum = 1 to writer.GetNumberOfPages() do
                writer.RemovePage(1)

            for flowModel in flowModels do
                let readerDocument = new ReaderDocument(flowModel.File)
                for pageNum = 1 to readerDocument.Reader.GetNumberOfPages() do
                    let page = readerDocument.Reader.GetPage(pageNum)
                    let writerPageResource = page.CopyTo(writer)
                    writer.AddPage(writerPageResource) |> ignore

                readerDocument.Reader.Close()

            { File = targetDocumentName 
              UserState = flowModels |> List.map (fun m -> m.UserState)
            }
            |> List.singleton


        |> FileOperation


    let mergeDocuments (f) =
        let args = f DocumentMergingArguments.DefalutValue

        fun flowModels ->

            if File.exists args.TargetDocumentPath && not args.Override then failwithf "target file %s already exists" args.TargetDocumentPath
            else File.delete args.TargetDocumentPath 

            let writer = new PdfDocument(new PdfWriter(args.TargetDocumentPath))
            let flow = mergeDocumentsInternal (args.TargetDocumentPath) writer

            let result = 
                flow.Value flowModels
            writer.Close()
            result
        |> FileOperation

    let splitDocumentToMany (f: DocumentSplitArguments -> DocumentSplitArguments)  =
        let args = f DocumentSplitArguments.DefalutValue
        if args.ChunkSize <= 0 then failwithf "Page num %d per document must be bigger than 1" args.ChunkSize
        fun (flowModels: FlowModel<'userState> list)  ->
            flowModels
            |> List.collect (fun flowModel ->
                let readerDocument = new ReaderDocument(flowModel.File)
                let reader = readerDocument.Reader

                let outputDirectory =
                    match args.OutputDirectory with 
                    | DocumentSplitOutputDirectoryOptions.ReaderDirectoryPath -> 
                        Path.getDirectory flowModel.File

                    | DocumentSplitOutputDirectoryOptions.CustomDirectoryPath directory ->
                        let directoryFullName = Path.GetFullPath directory
                        Directory.ensure directoryFullName
                        directoryFullName

                let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(flowModel.File)

                let newModels = 
                    reader
                    |> PdfDocument.getPages
                    |> List.chunkBySize(args.ChunkSize)
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

                        { File = fileFullPath 
                          UserState = flowModel.UserState }
                    )
                let newFiles = 
                    newModels
                    |> List.map (fun m -> 
                        m.File
                    )
                    |> String.concat "\n"
                Logger.info (sprintf "SPLIT document %s to %s" flowModel.File newFiles) 
                reader.Close()
                newModels
        )
        |> FileOperation

