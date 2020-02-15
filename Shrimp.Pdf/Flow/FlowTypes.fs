namespace Shrimp.Pdf



type FlowModel<'userState> =
    { File: string
      UserState: 'userState }




[<RequireQualifiedAccess>]
module FlowModel =

    let mapM mapping (flowModel: FlowModel<_>) =
        { File = flowModel.File 
          UserState = mapping flowModel.UserState }


[<RequireQualifiedAccess>]
type FlowName =
    | Default
    | Override of string
    | New of string
    | Disable

type FlowNameIndex =
    { Index: int 
      Name: string }

