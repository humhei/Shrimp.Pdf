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
type internal FlowName =
    | Usable of string
    | Overrided of string
    | None 