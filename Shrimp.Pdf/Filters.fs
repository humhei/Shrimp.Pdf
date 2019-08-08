namespace Atrous.Pdf
module Filters =
    open iText.Kernel.Pdf.Canvas.Parser.Data
    open Atrous.Pdf.Extensions
    open Types
    open Atrous.Utils
    open iText.Kernel.Geom

    let inline (<&>) (p1: 'a -> bool) (p2: 'a -> bool) =
        fun a -> p1 a && p2 a

    let inline (<|>) (p1: 'a -> bool) (p2: 'a -> bool) =
        fun a -> p1 a || p2 a

    let text (ps: (TextRenderInfo -> bool) list) = 
        fun (info:AbstractRenderInfo) ->
            if AbstractRenderInfo.isVisible info then
                match info with 
                | :? TextRenderInfo as info ->
                    ps |> List.forall (fun p ->
                        p info
                    )
                | _ -> false
            else false

    let path (ps: (PathRenderInfo -> bool) list) = 
        fun (info:AbstractRenderInfo) ->
            if AbstractRenderInfo.isVisible info then
                match info with 
                | :? PathRenderInfo as info ->
                    ps |> List.forall (fun p ->
                        p info
                    ) 
                | _ -> false
            else false

    let subPath (ps: (PathRenderInfo * Subpath -> bool) list) = 
        fun (info:AbstractRenderInfo,subpath) ->
            if AbstractRenderInfo.isVisible info then
                match info with 
                | :? PathRenderInfo as info ->
                    ps |> List.forall (fun p ->
                        p (info,subpath)
                    ) 
                | _ -> false
            else false

    let all (ps: ('a -> bool) list) = 
        fun (info:'a) ->
            ps |> List.forall (fun p ->
                p info
            ) 

    let visible (ps: (AbstractRenderInfo -> bool) list) = 
        AbstractRenderInfo.isVisible <&> all ps

    let choice (ps: ('a -> bool) list) =
        fun (info: 'a) ->
            ps |> List.fold (fun b p ->
                p info || b
            ) false

