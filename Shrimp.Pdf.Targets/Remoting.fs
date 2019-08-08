namespace Atrous.Pdfargets
module Remoting =
    open Fable.Remoting.DotnetClient
    open LegacyShared

    // specifies how the routes should be generated
    let routes = Route.routes

    // proxy: Proxy<IServer> 
    let proxy = Proxy.create<IBartenderApi> routes 
