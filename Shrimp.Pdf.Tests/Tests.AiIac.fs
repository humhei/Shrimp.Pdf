module Tests.AiIac
//open Expecto
//open Atrous.Pdf
//open LiteDB
//open Types
//open Fake.IO
//open Atrous.Ai

//let pass() = Expect.isTrue true "passed"
//let fail() = Expect.isTrue false "failed"

//let AIIacTests =
//  testList "Illustrator Iac Tests" [
//    ftestCase "ai to pdf" <| fun _ -> 
//        let src = root + @"\健乐\Exist\吊牌2正面.ai"
//        let dest = src |> Path.changeExtension ".pdf"
//        ExtendIac.saveCopyToPdf src dest
//        pass()
//  ]