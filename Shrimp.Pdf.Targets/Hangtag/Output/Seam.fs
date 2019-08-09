namespace Shrimp.Pdfargets.Hangtag.Output
[<RequireQualifiedAccess>]
module internal Seam =
    open Shrimp.Pdf.Types
    open Shrimp.Entities.Types
    open PageSelection
    open Shrimp.Pdf.Manipulates
    open Shrimp.Pdf.Targets.Actions
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
