namespace Atrous.Pdfargets.Hangtag.Output
[<RequireQualifiedAccess>]
module internal Seam =
    open Atrous.Pdf.Types
    open Atrous.Entities.Types
    open PageSelection
    open Atrous.Pdf.Manipulates
    open Atrous.Pdf.Targets.Actions
    open Manipulates

    let cutting (hangtag: Hangtag) (ms: Manipulate<_> list) =
        match hangtag.CuttingLine with 
        | None -> ms
        | Some (CuttingLine.Impress) ->
            ms @ 
                [ forCuttingLinePages retainImpressLineAI
                  forCuttingLinePages (addSeamText "刀版") ]

        | Some (CuttingLine.Both) | Some (CuttingLine.DieCutting) ->
            ms @ 
                [ forCuttingLinePages retatinCuttingLineAI
                  forCuttingLinePages (addSeamText "刀版") ]
