namespace Shrimp.Pdf
open iText.Kernel.Geom
open Akka.Configuration
open System.Reflection
open System.IO
#nowarn "0020"
open Shrimp.FSharp.Plus
open System
open Shrimp.Akkling.Cluster.Intergraction.Configuration
open iText.Kernel.Pdf
[<AutoOpen>]
module _ShpLayer =
    
    [<RequireQualifiedAccess>]
    module ShpPdfName =
        let ShpLayer = PdfName "ShpLayer" 
        let ShpLayerGroup = PdfName "ShpLayerGroup" 
        let ShpLayerOptions = PdfName "ShpLayerOptions" 
        let IsLocked = PdfName "IsLocked"
        let CuttingDieShpLayerInfosEnum = PdfName "CuttingDieShpLayerInfosEnum"

    [<RequireQualifiedAccess>]
    module PdfName =
        let ICCBased = PdfName "ICCBased"

    [<System.Flags>]
    type CuttingDieShpLayerInfosEnum =
        | CuttingDie = 1
        | CuttingDieDashLine = 2
        | TagInfos = 4

    type ShpLayerOptions =
        { IsLocked: bool }
    with 
        static member DefaultValue = { IsLocked = false }

        member x.AsPdfObject() =
            match x = ShpLayerOptions.DefaultValue with 
            | true -> None
            | false -> 
                let dict = PdfDictionary()
                dict.Put(ShpPdfName.IsLocked, PdfBoolean x.IsLocked)
                Some dict

        static member OfPdfObject(pdfDict: PdfDictionary) =
            { IsLocked = pdfDict.GetAsBool(ShpPdfName.IsLocked).Value}

    [<RequireQualifiedAccess>]
    type ShpLayer =
        | Bk_XObjectOnly_Case of name: string        * ops: ShpLayerOptions
        | Fr_XObjectOnly_Case of name: string        * ops: ShpLayerOptions
        | BK_Case             of name: string        * ops: ShpLayerOptions
        | Foreground_Case     of name: string        * ops: ShpLayerOptions
        | Content_Case        of name: string option * ops: ShpLayerOptions        
        | Pixel_Case          of name: string        * ops: ShpLayerOptions
        | CuttingDie_Case     of SkipComparation_Serializable<CuttingDieShpLayerInfosEnum> * ops: ShpLayerOptions
        | BkSolid_Case        of ops: ShpLayerOptions
        | CompoundPath_Case   of ops: ShpLayerOptions
        | ClippingPath_Case   of ops: ShpLayerOptions
        | SeamInfo_Case       of ops: ShpLayerOptions
        | ImposedData_Case    of ops: ShpLayerOptions    
        | PageNumber_Case     of ops: ShpLayerOptions 
    with 
        static member Bk_XObjectOnly (name: string        , ?ops: ShpLayerOptions                                         )      =
            Bk_XObjectOnly_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member Fr_XObjectOnly (name: string        , ?ops: ShpLayerOptions                                         )      =
            Fr_XObjectOnly_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member BK             (name: string        , ?ops: ShpLayerOptions                                         )      =
            BK_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member Foreground     (name: string        , ?ops: ShpLayerOptions                                         )      =
            Foreground_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member Content        (?name: string , ?ops: ShpLayerOptions                                         )      =
            Content_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member Pixel          (name: string        , ?ops: ShpLayerOptions                                         )      =
            Pixel_Case(name, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member CuttingDie     (enum: CuttingDieShpLayerInfosEnum, ?ops: ShpLayerOptions   ) =
            CuttingDie_Case(SkipComparation_Serializable enum, ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member BkSolid        (?ops: ShpLayerOptions)                                                                =
            BkSolid_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member CompoundPath   (?ops: ShpLayerOptions)                                                                =
            CompoundPath_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)
        static member ClippingPath   (?ops: ShpLayerOptions)                                                          =
            ClippingPath_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)

        static member SeamInfo  (?ops: ShpLayerOptions)                                                          =
            SeamInfo_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)

        static member ImposedData  (?ops: ShpLayerOptions)                                                          =
            ImposedData_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)

        static member PageNumber  (?ops: ShpLayerOptions)                                                          =
            PageNumber_Case(ops = defaultArg ops ShpLayerOptions.DefaultValue)


        member x.Options =
            match x with
            | Bk_XObjectOnly_Case (_, v) -> v
                
            | Fr_XObjectOnly_Case (_, v) -> v

            | BK_Case (_, v) -> v

            | Foreground_Case (_, v) -> v

            | Content_Case (_, v) -> v

            | Pixel_Case (_, v) -> v

            | CuttingDie_Case (_, v) -> v

            | BkSolid_Case v -> v

            | CompoundPath_Case v -> v

            | ClippingPath_Case v -> v

            | SeamInfo_Case v -> v
            | ImposedData_Case v -> v
            | PageNumber_Case v -> v

        member x.DefaultLayerName() =
            match x with
            | Bk_XObjectOnly_Case (v, _) -> v
                
            | Fr_XObjectOnly_Case (v, _) -> v

            | BK_Case (v, _) -> v

            | Foreground_Case (v, _) -> v

            | Content_Case (v, _) -> defaultArg v "Content"

            | Pixel_Case (v, _) -> v

            | CuttingDie_Case (enum, _) -> "CuttingDie"

            | BkSolid_Case _ -> nameof(ShpLayer.BkSolid)

            | CompoundPath_Case _ -> nameof(ShpLayer.CompoundPath)

            | ClippingPath_Case _ -> nameof(ShpLayer.ClippingPath)

            | SeamInfo_Case _ -> nameof(ShpLayer.SeamInfo)
            | ImposedData_Case _ -> nameof(ShpLayer.ImposedData)
            | PageNumber_Case _ -> nameof(ShpLayer.PageNumber)


        static member OfPdfObject(pdfObject: PdfObject) =
            let shpLayer = pdfObject :?> PdfDictionary
            let kind = shpLayer.GetAsString(PdfName.Type)
            let ops = 
                match shpLayer.ContainsKey ShpPdfName.ShpLayerOptions with 
                | true -> 
                    shpLayer.GetAsDictionary(ShpPdfName.ShpLayerOptions)
                    |> ShpLayerOptions.OfPdfObject
                    |> Some

                | false -> None 

            let name =  
                match shpLayer.ContainsKey(PdfName.Name) with 
                | false -> None
                | true -> 
                    shpLayer.GetAsString(PdfName.Name).GetValue()
                    |> Some
          

            match kind.GetValue() with 
            | nameof(ShpLayer.Bk_XObjectOnly) -> 
                ShpLayer.Bk_XObjectOnly (name.Value, ?ops = ops)

            | nameof ShpLayer.Fr_XObjectOnly -> ShpLayer.Fr_XObjectOnly(name.Value, ?ops = ops)
            | nameof ShpLayer.BK             -> ShpLayer.BK            (name.Value, ?ops = ops)
            | nameof ShpLayer.Foreground     -> ShpLayer.Foreground    (name.Value, ?ops = ops)
            | nameof ShpLayer.Content        -> ShpLayer.Content       (?name = name, ?ops = ops)
            | nameof ShpLayer.Pixel          -> ShpLayer.Pixel         (name.Value, ?ops = ops)
            | nameof ShpLayer.CuttingDie     ->  
                
                let enum_int = shpLayer.GetAsInt(ShpPdfName.CuttingDieShpLayerInfosEnum).Value
                let enum = enum enum_int
                ShpLayer.CuttingDie (enum, ?ops = ops)

            | nameof ShpLayer.BkSolid        -> ShpLayer.BkSolid(?ops = ops)     
            | nameof ShpLayer.CompoundPath   -> ShpLayer.CompoundPath(?ops = ops)  
            | nameof ShpLayer.ClippingPath   -> ShpLayer.ClippingPath(?ops = ops)  
            | nameof ShpLayer.SeamInfo   -> ShpLayer.SeamInfo(?ops = ops)  
            | nameof ShpLayer.ImposedData   -> ShpLayer.ImposedData(?ops = ops)  
            | nameof ShpLayer.PageNumber   -> ShpLayer.PageNumber(?ops = ops)  
            | _ -> failwithf "Cannot convert %A to ShpLayer" pdfObject

        member x.AsPdfObject() =
            let createPdfDict (ops: ShpLayerOptions) f =
                let dict = PdfDictionary()
                match ops.AsPdfObject() with 
                | None -> ()
                | Some dict -> 
                    dict.Put(ShpPdfName.ShpLayerOptions, dict)
                    |> ignore

                f dict |> ignore
                dict

            createPdfDict x.Options (fun dict ->
                match x with 
                | Bk_XObjectOnly_Case (v, _) -> 
                    let name = nameof(ShpLayer.Bk_XObjectOnly)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(PdfName.Name, PdfString v)
                    
                | Fr_XObjectOnly_Case (v, _) -> 
                    let name = nameof(ShpLayer.Fr_XObjectOnly)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(PdfName.Name, PdfString v)

                | BK_Case (v, _) -> 
                    let name = nameof(ShpLayer.BK)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(PdfName.Name, PdfString v)

                | Foreground_Case (v, _) -> 
                    let name = nameof(ShpLayer.Foreground)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(PdfName.Name, PdfString v)

                | Content_Case (v, _) -> 
                    let name = nameof(ShpLayer.Content)
                    dict.Put(PdfName.Type, PdfString name)
                    match v with 
                    | Some v ->
                        dict.Put(PdfName.Name, PdfString v)

                    | None -> dict

                | Pixel_Case (v, _) -> 
                    let name = nameof(ShpLayer.Pixel)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(PdfName.Name, PdfString v)

                | CuttingDie_Case (enum, _) -> 
                    
                    let name = nameof(ShpLayer.CuttingDie)
                    dict.Put(PdfName.Type, PdfString name)
                    dict.Put(ShpPdfName.CuttingDieShpLayerInfosEnum, PdfNumber(int enum.Value))

                | BkSolid_Case _ -> 
                    let name = nameof(ShpLayer.BkSolid)
                    dict.Put(PdfName.Type, PdfString name)

                | CompoundPath_Case _ -> 
                    let name = nameof(ShpLayer.CompoundPath)
                    dict.Put(PdfName.Type, PdfString name)

                | ClippingPath_Case _ -> 
                    let name = nameof(ShpLayer.ClippingPath)
                    dict.Put(PdfName.Type, PdfString name)

                | SeamInfo_Case _ -> 
                    let name = nameof(ShpLayer.SeamInfo)
                    dict.Put(PdfName.Type, PdfString name)

                | ImposedData_Case _ -> 
                    let name = nameof(ShpLayer.ImposedData)
                    dict.Put(PdfName.Type, PdfString name)

                | PageNumber_Case _ -> 
                    let name = nameof(ShpLayer.PageNumber)
                    dict.Put(PdfName.Type, PdfString name)
            )






    type PdfDictionary with 

        member x.PutShpLayerGroup(isGroup: bool) =
            x.Put(ShpPdfName.ShpLayerGroup, PdfBoolean isGroup)
            |> ignore


        member x.PutShpLayer(shpLayer: ShpLayer) =
            match x.ContainsKey ShpPdfName.ShpLayerGroup with 
            | true -> ()
            | false -> 
                x.Put(ShpPdfName.ShpLayer, shpLayer.AsPdfObject())
                |> ignore

module Constants =

    type private AssemblyFinder = AssemblyFinder

    let private config = 
        ConfigurationFactory
            .FromResource<AssemblyFinder>("Shrimp.Pdf.Extensions.reference.conf")
        |> Configuration.fallBackByApplicationConf

    /// default is 0.1
    let tolerance = 
        config.GetDouble("shrimp.pdf.tolerance")


    let textInfoHeightRedirectPercentage = 
        config.GetDouble("shrimp.pdf.textInfoHeightRedirectPercentage")

    let [<Literal>] MAXIMUM_MM_WIDTH = 5080.

    let [<Literal>] UNTITLED = "untitled"


    module Operators =

        let (|EQ|_|) a b =
            if a = b then Some ()
            else None


        ///closepath, fill, stroke
        ///
        ///Close,   fill,   and   stroke   path   using   nonzero   winding number rule
        let [<Literal>] b = "b"
        
        ///fill, stroke
        ///
        ///Fill and stroke path using nonzero winding number rule
        let [<Literal>] B = "B"
        
        ///closepath, eofill, stroke
        ///
        ///Close, fill, and stroke path using even-odd rule
        let [<Literal>] ``b*`` = "b*"
        
        ///eofill, stroke
        ///
        ///Fill and stroke path using even-odd rule
        let [<Literal>] ``B*`` = "B*"
        
        ///
        ///
        ///(PDF 1.2) Begin marked-content sequence with property list
        let [<Literal>] BDC = "BDC"
        
        ///
        ///
        ///Begin inline image object
        let [<Literal>] BI = "BI"
        
        ///
        ///
        ///(PDF 1.2) Begin marked-content sequence
        let [<Literal>] BMC = "BMC"
        
        ///
        ///
        ///Begin text object
        let [<Literal>] BT = "BT"
        
        ///
        ///
        ///(PDF 1.1) Begin compatibility section
        let [<Literal>] BX = "BX"
        
        ///curveto
        ///
        ///Append curved segment to path (three control points)
        let [<Literal>] c = "c"
        
        ///concat
        ///
        ///Concatenate matrix to current transformation matrix
        let [<Literal>] cm = "cm"
        
        ///setcolorspace
        ///
        ///(PDF 1.1) Set color space for stroking operations
        let [<Literal>] CS = "CS"
        
        ///setcolorspace
        ///
        ///(PDF 1.1) Set color space for nonstroking operations
        let [<Literal>] cs = "cs"
        
        ///setdash
        ///
        ///Set line dash pattern
        let [<Literal>] d = "d"
        
        ///setcharwidth
        ///
        ///Set glyph width in Type 3 font
        let [<Literal>] d0 = "d0"
        
        ///setcachedevice
        ///
        ///Set glyph width and bounding box in Type 3 font
        let [<Literal>] d1 = "d1"
        
        ///
        ///
        ///Invoke named XObject
        let [<Literal>] Do = "Do"
        
        ///
        ///
        ///(PDF 1.2) Define marked-content point with property list
        let [<Literal>] DP = "DP"
        
        ///
        ///
        ///End inline image object
        let [<Literal>] EI = "EI"
        
        ///
        ///
        ///(PDF 1.2) End marked-content sequence
        let [<Literal>] EMC = "EMC"
        
        ///
        ///
        ///End text object
        let [<Literal>] ET = "ET"
        
        ///
        ///
        ///(PDF 1.1) End compatibility section
        let [<Literal>] EX = "EX"
        
        ///fill
        ///
        ///Fill path using nonzero winding number rule
        let [<Literal>] f = "f"
        
        ///fill
        ///
        ///Fill path using nonzero winding number rule (obsolete)
        let [<Literal>] F = "F"
        
        ///eofill
        ///
        ///Fill path using even-odd rule
        let [<Literal>] ``f*`` = "f*"
        
        ///setgray
        ///
        ///Set gray level for stroking operations
        let [<Literal>] G = "G"
        
        ///setgray
        ///
        ///Set gray level for nonstroking operations
        let [<Literal>] g = "g"
        
        ///
        ///
        ///(PDF 1.2) Set parameters from graphics state parameter dictionary
        let [<Literal>] gs = "gs"
        
        ///closepath
        ///
        ///Close subpath
        let [<Literal>] h = "h"
        
        ///setflat
        ///
        ///Set flatness tolerance
        let [<Literal>] i = "i"
        
        ///
        ///
        ///Begin inline image data
        let [<Literal>] ID = "ID"
        
        ///setlinejoin
        ///
        ///Set line join style
        let [<Literal>] j = "j"
        
        ///setlinecap
        ///
        ///Set line cap style
        let [<Literal>] J = "J"
        
        ///setcmykcolor
        ///
        ///Set CMYK color for stroking operations
        let [<Literal>] K = "K"
        
        ///setcmykcolor
        ///
        ///Set CMYK color for nonstroking operations
        let [<Literal>] k = "k"
        
        ///lineto
        ///
        ///Append straight line segment to path
        let [<Literal>] l = "l"
        
        ///moveto
        ///
        ///Begin new subpath
        let [<Literal>] m = "m"
        
        ///setmiterlimit
        ///
        ///Set miter limit
        let [<Literal>] M = "M"
        
        ///
        ///
        ///(PDF 1.2) Define marked-content point
        let [<Literal>] MP = "MP"
        
        ///
        ///
        ///End path without filling or stroking
        let [<Literal>] n = "n"
        
        ///gsave
        ///
        ///Save graphics state
        let [<Literal>] q = "q"
        
        ///grestore
        ///
        ///Restore graphics state
        let [<Literal>] Q = "Q"
        
        ///
        ///
        ///Append rectangle to path
        let [<Literal>] re = "re"
        
        ///setrgbcolor
        ///
        ///Set RGB color for stroking operations
        let [<Literal>] RG = "RG"
        
        ///setrgbcolor
        ///
        ///Set RGB color for nonstroking operations
        let [<Literal>] rg = "rg"
        
        ///
        ///
        ///Set color rendering intent
        let [<Literal>] ri = "ri"
        
        ///closepath, stroke
        ///
        ///Close and stroke path
        let [<Literal>] s = "s"
        
        ///stroke
        ///
        ///Stroke path
        let [<Literal>] S = "S"
        
        ///setcolor
        ///
        ///(PDF 1.1) Set color for stroking operations
        let [<Literal>] SC = "SC"
        
        ///setcolor
        ///
        ///(PDF 1.1) Set color for nonstroking operations
        let [<Literal>] sc = "sc"
        
        ///setcolor
        ///
        ///(PDF 1.2)  Set  color  for stroking  operations  (ICCBased and special colour spaces)
        let [<Literal>] SCN = "SCN"
        
        ///setcolor
        ///
        ///(PDF    1.2)    Set    color    for    nonstroking    operations (ICCBased and special colour spaces)
        let [<Literal>] scn = "scn"
        
        ///shfill
        ///
        ///(PDF 1.3) Paint area defined by shading pattern
        let [<Literal>] sh = "sh"
        
        ///
        ///
        ///Move to start of next text line
        let [<Literal>] ``T*`` = "T*"
        
        ///
        ///
        ///Set character spacing
        let [<Literal>] Tc = "Tc"
        
        ///
        ///
        ///Move text position
        let [<Literal>] Td = "Td"
        
        ///
        ///
        ///Move text position and set leading
        let [<Literal>] TD = "TD"
        
        ///selectfont
        ///
        ///Set text font and size
        let [<Literal>] Tf = "Tf"
        
        ///show
        ///
        ///Show text
        let [<Literal>] Tj = "Tj"
        
        ///
        ///
        ///Show text, allowing individual glyph positioning
        let [<Literal>] TJ = "TJ"
        
        ///
        ///
        ///Set text leading
        let [<Literal>] TL = "TL"
        
        ///
        ///
        ///Set text matrix and text line matrix
        let [<Literal>] Tm = "Tm"
        
        ///
        ///
        ///Set text rendering mode
        let [<Literal>] Tr = "Tr"
        
        ///
        ///
        ///Set text rise
        let [<Literal>] Ts = "Ts"
        
        ///
        ///
        ///Set word spacing
        let [<Literal>] Tw = "Tw"
        
        ///
        ///
        ///Set horizontal text scaling
        let [<Literal>] Tz = "Tz"
        
        ///curveto
        ///
        ///Append curved segment to path (initial point replicated)
        let [<Literal>] v = "v"
        
        ///setlinewidth
        ///
        ///Set line width
        let [<Literal>] w = "w"
        
        ///clip
        ///
        ///Set clipping path using nonzero winding number rule
        let [<Literal>] W = "W"
        
        ///eoclip
        ///
        ///Set clipping path using even-odd rule
        let [<Literal>] ``W*`` = "W*"
        
        ///curveto
        ///
        ///Append curved segment to path (final point replicated)
        let [<Literal>] y = "y"
        
        

[<AutoOpen>]
module Operators =
    open Constants



    type FsPdfObjectID =
        { ObjNumber: int 
          GenNumber: int }

    
    [<Struct>]
    type InfoContainerID =
        | Page
        | XObject of FsPdfObjectID

    type SpawnablePdfObjectID =
        { ObjNumber: int 
          GenNumber: int
          IsSpawned: bool }
    with 
        static member OfPdfObjectID(id: FsPdfObjectID, ?isSpawnded) =
            { ObjNumber = id.ObjNumber
              GenNumber = id.GenNumber 
              IsSpawned = defaultArg isSpawnded false }

    let hashNumberOfPdfIndirectReference(pdfIndirectReference: PdfIndirectReference) =
        { ObjNumber = pdfIndirectReference.GetObjNumber() 
          GenNumber = pdfIndirectReference.GetGenNumber() }


    /// approximately equal to 
    /// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    let (@=) a b =
        (abs (a - b)) < tolerance

    ///// approximately bigger or equal to 
    ///// benchmark by (CONFIG: shrimp.pdf.tolerance (default is 0.1))
    //let (>=@) a b =
    //    a > b
    //    || (abs (a - b)) < tolerance

    /// unSerializable
    type NearbyPX (v: float, tolerance) =
        inherit CustomComparableBase<float>(v, fun a b ->
            if (abs (a-b)) <= tolerance
            then 0
            else compare a b
        )

        member x.Value = v

        new (v) =
            NearbyPX(v, tolerance)
            

    /// defaultConversion: mm to user unit
    let mm (mm: float) =
        mm / 25.4 * 72.


    let CM (cm: float) =
        mm cm * 10.

    let mmZ (mm: float) =
        mm / 25.4 * 72.
        |> ``ufloat>0``

    /// defaultConversion: userUnit to mm
    let userUnitToMM (userUnit: float) =
        userUnit / 72. * 25.4



    let inchToMM (inch: float) =
        inch * 25.4

    let inch (inch: float) =
        inch * 72.