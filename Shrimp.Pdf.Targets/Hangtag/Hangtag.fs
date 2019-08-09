
namespace Shrimp.Pdf.Targets.Hangtag

open Shrimp.Pdf.Targets.Hangtag.Output

[<RequireQualifiedAccess>]
module Hangtag =
    let makeSure = MakeSure.run
    let proof = Proof.run
    let output = Output.run
