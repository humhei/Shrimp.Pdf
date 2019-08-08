namespace Atrous.Pdf

module Scripts = 
    open System
    open System.Reflection
    let usingScript script jsObj (f: Type -> unit) =
        let t = jsObj.GetType()
        t.InvokeMember(
            "setAction",
            BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Instance,
            null,
            jsObj,
            [|box "WillSave"; box script|]
        ) |> ignore
        f t
        t.InvokeMember(
            "setAction",
            BindingFlags.InvokeMethod ||| BindingFlags.Public ||| BindingFlags.Instance,
            null,
            jsObj,
            [|box "WillSave"; box ""|]
        ) |> ignore
    let convertToOutlinesScript = """ 
        var oProfile = Preflight.getProfileByName("Convert fonts to outlines");
        var oThermometer = app.thermometer;

        var myPreflightResult = this.preflight( oProfile, false, oThermometer);

        if( myPreflightResult.numErrors > 0 ) {
        console.println( "Preflight found " + myPreflightResult.numErrors + " Errors.");
        } """