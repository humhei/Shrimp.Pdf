namespace Atrous.Entities


open LiteDB
open LiteDB.FSharp.Extensions
open LiteDB.FSharp.Linq
open LiteDB.FSharp
open Types
open Atrous
open Atrous.Utils

[<RequireQualifiedAccess>]
module LiteRepository = 

    let retrieveTag companyName tagName (db: LiteRepository) =
        db 
        |> LiteRepository.query<Tag>
        |> LiteQueryable.expand(Expr.prop (fun t -> t.Products))
        |> LiteQueryable.tryFind(Expr.prop(fun t -> t.Company.Name = companyName && t.Name = tagName))

    let retrieveProducts (tag:Tag) (db: LiteRepository) =
        db 
        |> LiteRepository.query<Product>
        |> LiteQueryable.expand (Expr.prop (fun p -> p.Orders))
        |> LiteQueryable.where (Expr.prop (fun p -> tag.Products |> List.map(fun p -> p.Id) |> List.contains p.Id ))
        |> LiteQueryable.toList


    let retrieveOrders tagName (orders: Order list) (db: LiteRepository) =
        let names = orders |> List.map (fun o -> o.Name)
        let pre o =
            o.TagName = tagName 
            && names |> List.contains o.Name
        db 
        |> LiteRepository.query<Order>
        |> LiteQueryable.where(Expr.prop (fun o -> 
            pre o
        ))
        |> LiteQueryable.toList

    let retrieveOrdersByQueriedProduct (product: Product) (db: LiteRepository) =
            
        db 
        |> LiteRepository.query<Product>
        |> LiteQueryable.where(Expr.prop (fun p -> 
            p.Id = product.Id
        ))
        |> LiteQueryable.expand(Expr.prop (fun p -> p.Orders))
        |> LiteQueryable.first
        |> fun prod -> prod.Orders

    let upsertOrders (orders: Order list) (queriedOrders: Order list) db =
        let add =
            let names = queriedOrders |> List.map (fun o -> o.Name)
            let newOrders = 
                orders |> List.filter (fun o ->
                    names |> List.contains o.Name |> not
                )
            newOrders |> List.map (fun o ->
                LiteRepository.updateItem o db |> ignore
            )

        let newOrders =
            queriedOrders 
            |> List.mapi (fun i qo ->
                if not (ReflectionAdapters.isPropertyStruturalEqualityForItems qo.Items orders.[i].Items) then
                    let newOrder = { qo with Items = orders.[i].Items }
                    LiteRepository.updateItem newOrder db |> ignore
                    printfn "Updated order %A with items" qo 
                    newOrder
                else qo
            )

        newOrders
    let upsertProducts tagName (products: Product list) (queriedProducts: Product list) (db:LiteRepository) =

        let delete =
            let names = products |> List.map (fun prod -> prod.Name)
            let deletedProducts = queriedProducts |> List.filter (fun qp ->
                not (names |> List.contains qp.Name)
            )

            let ids = deletedProducts |> List.map (fun dp -> dp.Id)
            db.Delete<Product>((fun (product:Product) -> List.contains product.Id ids)) |> ignore
            deletedProducts |> List.iter (fun dp -> 
                printfn "deleted product %s" dp.Name
            )
            deletedProducts

        let add = 
            if products.IsEmpty then []
            else 
                let qNames = queriedProducts |> List.map (fun qp -> qp.Name)
                let addedProducteds = products |> List.filter (fun prod ->
                    qNames |> List.contains prod.Name |> not
                )
                if not addedProducteds.IsEmpty then 
                    let orders = retrieveOrders tagName products.[0].Orders db
                    let newAddedProds = addedProducteds |> List.map (fun prod -> { prod with Orders = orders })
                    LiteRepository.insertItems newAddedProds db |> ignore
                    newAddedProds |> List.iter (fun ap ->
                        printfn "Inserted product %s" ap.Name
                    )
                    newAddedProds
                else []



        let cus =
            let delIds =  delete |> List.map (fun del -> del.Id)
            queriedProducts @ add 
            |> List.filter (fun (prod: Product) -> 
                not (delIds |> List.contains (prod.Id))
            )
            |> List.distinctBy (fun prod -> prod.Id) 
            
        let updateOrders = 
            match List.tryHead cus with 
            | Some cu ->
                let queriedOrders = retrieveOrders tagName products.[0].Orders db
                upsertOrders products.[0].Orders queriedOrders db
            | None -> []


        cus 
        |> List.mapi (fun i cu ->
            let rec transform cu (fs) isUpdated =  
                match fs with 
                | h :: t ->
                    let f,newGenerator = h
                    let oldEle = f cu
                    let newEle = f products.[i]
                    if oldEle <> newEle
                    then 
                        printfn "updated product %A with new product element %A and old product element %A" cu newEle oldEle
                        let updated = newGenerator newEle cu
                        transform updated t true
                    else 
                        transform cu t isUpdated
                | [] -> cu,isUpdated
            let fs =
                [
                    (fun product -> box product.Kind),(fun kind (product: Product) -> {product with Kind = unbox kind})
                ]
            transform cu fs false
            |> function 
                | cu,true ->
                    let updated = { cu with Orders = updateOrders }
                    LiteRepository.updateItem updated db |> ignore
                    updated
                | cu,false ->
                    cu
        )


    let insertTag (tag: Tag) (db:LiteRepository) =
        let products =
            tag.Products
            |> List.tryHead
            |> function
                | Some prod ->
                    let orders = prod.Orders

                    db
                    |> LiteRepository.insertItems orders 
                    |> ignore
                        
                    orders 
                    |> List.iter( fun o ->
                        printfn "inserted order %s" o.Name
                    )

                    tag.Products |> List.map (fun prod -> { prod with Orders = orders })
                | None -> []
        db 
        |> LiteRepository.insertItems products
        |> LiteRepository.insertItem { tag with Products = products }


    let upsertTag (tag: Tag) (db:LiteRepository) =
        if tag.Id = 0 then 
            db 
            |> retrieveTag tag.Company.Name tag.Name
            |> function
                | Some queriedTag ->
                    let newProducts = upsertProducts tag.Name tag.Products queriedTag.Products db
                            
                    if tag.Products.Length = queriedTag.Products.Length then 
                        if List.forall2 (fun (p1:Product) (p2:Product) -> 
                            p1.Name = p2.Name) tag.Products queriedTag.Products
                        then 
                            printfn "skip upserting tag %s" tag.Name
                        else 
                            LiteRepository.updateItem<Tag> {queriedTag with Products = newProducts} db |> ignore
                    else LiteRepository.updateItem<Tag> {queriedTag with Products = newProducts} db |> ignore
                | None -> 
                    printfn "inserting tag %s" tag.Name
                    insertTag tag db |> ignore
        else failwith "Not implemented"
