namespace LegacyShared


module Route =
    /// Defines how routes are generated on server and mapped from client
    let hostName = "http://localhost:9090"

    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

    let routes typeName methodName = 
        builder typeName methodName |> sprintf "%s%s" hostName
        

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IBartenderApi =
    { print : string -> (string * string) list -> string -> Async<string list> }
