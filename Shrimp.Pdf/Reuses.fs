namespace Atrous.Pdf

module Reuses =
    open iText.Kernel.Pdf
    open Types
    open Atrous.Pdf.Extensions
    open iText.Kernel.Geom
    open iText.Kernel.Pdf.Canvas
    open Atrous.Utils
    open Atrous.Extensions
    open Atrous

    let private assignPlatesWithPages (arg: ImposingArgument) (dest: PdfDocument) (pages: PdfPage list) = 
        let hspaces = Seq.repeatToInfinite arg.HSpaces
        let vspaces = Seq.repeatToInfinite arg.VSpaces
        let margin = arg.Margin

        let assignRow (units: ImposerCell list) =
            let units,x = 
                let l = units.Length
                units |> List.indexed |> List.mapFold (fun accum (i,u) ->
                    let space = 
                        if i = l - 1 then 0.
                        else Seq.item i hspaces

                    let preSpace = 
                        if i = 0 then 0. 
                        else Seq.item (i-1) hspaces

                    { u with 
                        X = accum 
                        Space = space
                        PreSpace = preSpace
                        },accum + space + u.Size.Width
                ) 0.

            let units = 
                let l = units.Length
                match l with 
                | 0 | 1 -> 
                    units
                | l when l > 1 ->
                    units |> List.mapi (fun i u ->
                        let pos =
                            if i = 0 then ImposerPosition.First
                            elif i = l - 1 then ImposerPosition.Last
                            else ImposerPosition.Middle
                        { u with Position = pos }
                    )
                | _ -> failwithf "invaid units length %d" l

            {
                Units = units
                Width = 
                    let lastItem =
                        units |> List.last
                    lastItem.X + lastItem.Size.Width
                Y = 0.
                Height = units |> List.map(fun u -> u.Size.Height) |> List.max
                Position = ImposerPosition.FirstANDLast
                Space = 0.
                PreSpace = 0.
            }

        let assignPlate (rows: ImposerRow list) =
            let rows = 
                let l = rows.Length
                rows |> List.indexed |> List.mapFold (fun accum (i,r) ->
                    let space = 
                        if i = l - 1 then 0.
                        else Seq.item i vspaces

                    let preSpace = 
                        if i = 0 then 0. 
                        else Seq.item (i-1) vspaces
                    {
                        r with 
                            Y = accum
                            Space = space
                            PreSpace = preSpace
                    },accum - r.Height - space
                ) 0.
                |> fst
            let rows = 
                let l = rows.Length
                match l with 
                | 0 | 1 -> rows
                | l when l > 1 ->
                    rows |> List.mapi (fun i r ->
                        let pos =
                            if i = 0 then ImposerPosition.First
                            elif i = l - 1 then ImposerPosition.Last
                            else ImposerPosition.Middle
                        { r with Position = pos }
                    )
                | _ -> Logger.invalidToken()

            {
                Rows = rows
                Height = 
                    let lastItem = List.last rows
                    -lastItem.Y + lastItem.Height
                Width = rows |> List.map(fun r -> r.Width) |> List.max
                Cropmark = arg.Cropmark
                DestDoc = dest
                Margin = margin
                Background = arg.Background
            }

        let colNums = Seq.repeatToInfinite arg.ColNums
        let rec loop (pages: PdfPage list) (units: ImposerCell list) (rows: ImposerRow list) (plates: ImposerTable list) = 
            match pages with 
            | [] -> 
                if units.Length = 0 then 
                    if rows.Length = 0 then plates
                    else plates @ [assignPlate rows]
                else 
                    let rows = [assignRow units]
                    plates @ [assignPlate rows]
            | h :: t ->
                let unit = 

                    let xobject = h.CopyAsFormXObject(dest)
                    let resource_rotate = xobject |> PdfFormXObject.getRotate
                    
                    let size =
                        match arg.DesiredSize with 
                        | Some size -> 
                            size
                        | None -> 
                            let box = 
                                if arg.UseBleed then h.GetTrimBox()
                                else h.GetBBox()
                            {
                                Height = box.GetHeightF()
                                Width = box.GetWidthF()
                            }
                            |> FsSize.rotate resource_rotate

                    let degree =Rotation.getDegree arg.RotateXObjectDegree

                    let size = 
                        size |> FsSize.rotate degree

                    
                    {
                        Page = h
                        X = 0.
                        Size = size
                        Degree = degree + resource_rotate
                        XObject = xobject
                        Position = ImposerPosition.FirstANDLast
                        Bleed = arg.UseBleed
                        Space = 0.
                        PreSpace = 0.
                    }
                                        
                let colNum = colNums |> Seq.item rows.Length
                if arg.IsRepeated then 
                    let pageSize =
                        match arg.Background with 
                        | Background.Size size -> size
                        | Background.None -> FsSize.maximalBackground
                        | _ -> failwith "Not Implemented"
                    
                    let units = (units @ [unit])
                    let isInPageWidth = 
                        let newWidth =
                            let spaceSum = Seq.take units.Length hspaces |> Seq.sum
                            let widthSum = units @ [unit] |> List.sumBy (fun u -> u.Size.Width)
                            widthSum + margin.Left + margin.Right + spaceSum
                        not (newWidth > pageSize.Width)

                    if units.Length <> colNum && isInPageWidth then loop (h :: t) units rows plates
                    else 
                        let rows = rows @ [assignRow units]
                        let isInPageHeight =
                            let newHeight = 
                                let spaceSum = Seq.take rows.Length vspaces |> Seq.sum
                                let heightSum = (rows |> List.sumBy (fun r -> r.Height)) + unit.Size.Height
                                heightSum + margin.Bottom + margin.Top + spaceSum
                            not (newHeight > pageSize.Height)

                        if rows.Length <> arg.RowNum && isInPageHeight then loop (h :: t) [] rows plates
                        else 
                            let plates = plates @ [assignPlate rows]
                            loop t [] [] plates
                else
                    let units = (units @ [unit])
                    if units.Length <> colNum then loop t units rows plates
                    else 
                        let rows = rows @ [assignRow units]
                        if rows.Length <> arg.RowNum then loop t [] rows plates
                        else 
                            let plates = plates @ [assignPlate rows]
                            loop t [] [] plates
                                    
        loop pages [] [] []

    let multiple (f: PdfDocument -> FlowModel<'userState> -> (FlowModel<'userState> * PdfDocument) list) =
        Flow.Reuse <|
            fun (model: FlowModel<'userState>) ->
                let src = new PdfDocument(new PdfReader(model.Path))
                let models,dests = f src model |> List.unzip
                src.Close()
                dests |> List.iter (fun doc -> doc.Close())
                models

    let single (f: SplitterDocument -> FlowState<'userState> -> FlowState<'userState>) =
        Flow.Reuse <|
            fun (model: FlowModel<'userState>) ->
                let src = model.Path
                let dest = tmp src
                let doc = SplitterDocument.create src dest
                let newState = f doc model.FlowState
                doc.Src.Close()
                doc.Dest.Close()
                draft src dest
                [{model with FlowState = newState}]
    
    let private simpleImpose arg =
        single <| fun doc state ->
            let maxValue = 9999
            let arg = 
                {
                    arg with 
                        ColNums = 
                            arg.ColNums
                            |> List.map(fun colNum ->
                                if colNum = 0 then maxValue else colNum
                            )
                        RowNum = if arg.RowNum = 0 then maxValue else arg.RowNum
                }
            let src = doc.Src
            let dest = doc.Dest
            let pages = src |> PdfDocument.getAllPages
            let plates = assignPlatesWithPages arg dest pages 
            plates |> Seq.iter ImposerTable.draw
            {
                state with Tables = plates
            }

    let fixPosition() : Flow<_> =
        single <| fun doc state -> 
            let src = doc.Src
            let dest = doc.Dest
            src |> PdfDocument.getAllPages |> List.iter (fun page ->
                let pageSize,x,y = 
                    let pageSize = page.GetPageSize()
                    pageSize,
                    pageSize.GetX(),
                    pageSize.GetY()
                let xobject = page.CopyAsFormXObject(dest)
                let destPage = 
                    let rotatedPageSize = 
                        let size = page.GetPageSizeWithRotation()
                        let width = size.GetWidth()
                        let height = size.GetHeight()
                        new PageSize(width,height)
                    dest.AddNewPage(rotatedPageSize)
                let canvas = new PdfCanvas(destPage)

                let degree = 
                    match xobject.GetPdfObject().GetAsNumber(PdfName.Rotate) with 
                    | null -> 
                        0.
                    | rotation -> rotation.GetValue()
                let tf = new AffineTransform()
                tf.Rotate(radians -degree,float x,float y)
                let setBleed =
                    let cropBox = page.GetCropBox() |> tf.TransformRectangle
                    let trimBox = page.GetTrimBox() |> tf.TransformRectangle
                    if  not (Rectangle.equal cropBox trimBox) then 
                        let x = trimBox.GetX() - cropBox.GetX()
                        let y = trimBox.GetY() - cropBox.GetY()
                        let rect = new Rectangle(x,y,trimBox.GetWidth(),trimBox.GetHeight())
                        destPage.SetTrimBox(rect) |> ignore
                let addToCanvas= 
                    let transformedRect = tf.TransformRectangle(pageSize)
                    let matrix = Array.create 6 0.f
                    tf.GetMatrix(matrix)
                    canvas.AddXObject
                        (xobject,matrix.[0],matrix.[1],matrix.[2],matrix.[3],
                            matrix.[4] - transformedRect.GetX(),
                            matrix.[5] - transformedRect.GetY()) 
                            |> ignore
                ()
            )
            state

    let impose (arg: ImposingArgument) : Flow<_> =
        Flow.Composite 
            [
                fixPosition()
                simpleImpose arg
            ]

    let imposeWith (f: ImposingArgument -> ImposingArgument) : Flow<_> =
        ImposingArgument.dummy |> f |> impose
    
    let imposeFB arg (flipWay: FlipWay) =
        Flow.Composite 
            [
                fixPosition()
                single <| fun doc state ->
                    match flipWay with
                    | FlipWay.FB ->
                        let pages = doc.Src |> PdfDocument.getAllPages
                        let fpages,bpages = pages |> List.splitList 
                        let bplates = assignPlatesWithPages arg doc.Dest bpages
                        let fplate = 
                            let fpage = (fpages |> List.head)
                            let arg = { arg with IsRepeated = true }
                            assignPlatesWithPages arg doc.Dest [fpage] |> List.exactlyOne
                        fplate :: bplates |> Seq.iter ImposerTable.draw
                        state
                    | FlipWay.HFlip -> 
                        let assignFB pages = 
                            let colNums = arg.ColNums |> Seq.map (fun n -> n / 2)
                            let rec assign (fpages: PdfPage list) (bpages: PdfPage list) index (pages: PdfPage list) =
                                let colNum = colNums |> Seq.item index
                                if fpages.Length < colNum || bpages.Length < colNum then 
                                    pages
                                else
                                    let pickedFPages,leftFPages = fpages |> List.splitAt colNum
                                    let pickedBPages,leftBPages = bpages |> List.splitAt colNum
                                    let pickedPages = pages @ pickedFPages @ List.rev pickedBPages
                                    assign leftFPages leftBPages (index + 1) pickedPages

                            let fpages,bpages = pages |> List.splitList
                            assign fpages bpages 0 []

                        let pages = doc.Src |> PdfDocument.getAllPages |> assignFB

                        let plate = assignPlatesWithPages arg doc.Dest pages |> List.exactlyOne
                        ImposerTable.draw plate
                        state
                    | _ -> 
                        failwith "Not implemented"
            ]
    let duplicatePagesWith (f : int list -> int list): Flow<_> =
        single <| fun doc md ->
            let src = doc.Src
            let serials = 
                [1 .. src.GetNumberOfPages()] |> f
            let dest = doc.Dest
            let tables = 
                match md.Tables with 
                | [] -> []
                | _ ->
                    serials
                    |> List.map (fun pageNum -> md.Tables.[pageNum-1])
            src.CopyPagesTo(Array.ofList serials,dest) |> ignore
            { md with Tables = tables }

    let duplicatePages (serials: int list) : Flow<_> =
        duplicatePagesWith (fun _ -> serials)

    let duplicateForEachPage number : Flow<_> =
        duplicatePagesWith (fun serials ->
            serials |> List.collect (List.replicate number)
        )

    let duplicatePagesForFirstPageOfFront() =
        duplicatePagesWith (fun pageNums -> 
            pageNums |> List.collect (fun pageNum ->
                if pageNum = 1 then List.replicate (pageNums.Length - 1) pageNum
                else [pageNum]
            )
        )

    let duplicateFirstPageToLast() =
        duplicatePagesWith (fun pageNums ->
            pageNums |> List.map (fun pageNum ->
                if pageNum = pageNums.Length then [pageNum;1]
                else [pageNum]
            )
            |> List.concat
        )

    let duplicateForPageNumber pageNum duplicateNum : Flow<_> =
        duplicatePagesWith (fun serials ->
            serials |> List.collect (fun serial ->
                if serial = pageNum then List.replicate duplicateNum serial 
                else [serial]
            )
        )

    let resize (s: FsSize) =
        impose 
            { ImposingArgument.dummy with 
                DesiredSize = Some s
                ColNums = [1]
                RowNum = 1 }

    let clockwise() =
        let arg = 
            {
                ColNums = [1]
                RowNum = 1
                Cropmark = None
                HSpaces = [0.]
                VSpaces = [0.]
                Margin = Margin.createSimple 0.
                UseBleed = false
                Background = Background.None
                RotateXObjectDegree = Rotation.None
                DesiredSize = None
                IsRepeated = false
            }
        impose arg


    let merge (externalPdf: string) =
        single <| fun doc model ->
            let extPdf = new PdfDocument(new PdfReader(externalPdf))
            PdfDocument.getAllPages extPdf
            @ PdfDocument.getAllPages doc.Src 
            |> List.iter (fun p -> 
                let copied = p.CopyTo(doc.Dest)
                doc.Dest.AddPage copied |> ignore
            )
            model

    let mergeFront (externalPdf: string) =
        single <| fun doc model ->
            let extPdf = new PdfDocument(new PdfReader(externalPdf))

            PdfDocument.getAllPages doc.Src 
            |> List.collect (fun p ->
                [extPdf.GetFirstPage();p]
            )
            |> List.iter (fun p -> 
                let copied = p.CopyTo(doc.Dest)
                doc.Dest.AddPage copied |> ignore
            )
            model

    let oneColumn() =
        imposeWith (fun arg ->
            { arg with ColNums = [1] }
        )
