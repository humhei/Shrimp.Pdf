module Tests.AcrobatIac
//open Expecto
//open Atrous.Pdf
//open Types
//open Fake.IO.FileSystemOperators


//let pass() = Expect.isTrue true "passed"
//let fail() = Expect.isTrue false "failed"

//let IacTests =
//  testList "Acrobat Iac Tests" [
//    ftestCase "imposing transfer" <| fun _ -> 
//        let src = materialDir </> "热转印.pdf"
//        let dest = materialDir </> "test.eps"
//        Iac.saveToEPS src dest 
//        pass()
//  ]