﻿[<AutoOpen>]
module Types
open Expecto
open Shrimp.Pdf.Reuses
open Shrimp.Pdf
open Shrimp.Pdf.Imposing
open Shrimp.Pdf.Parser
open Shrimp.Pdf.Colors
open iText.Kernel.Colors
open Shrimp.Pdf.Extensions
open Fake.IO
open System.IO
open System.Threading


let runWithBackup path flow =
    let newPath = Path.changeExtension ".tests.pdf" path
    File.Copy(path, newPath, true)
    run { UserState = (); File = newPath } flow

