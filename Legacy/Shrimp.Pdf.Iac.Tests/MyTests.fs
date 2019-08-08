module MyTests

open Expecto
open Atrous.Pdf
open Fake.IO
open Fake.IO.FileSystemOperators
let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let dir = Path.getFullName "datas"
let myTests =
  testList "MyTests" [
    testCase "valid ean13 test 1" <| fun _ -> 
        let path = dir </> "saveTo.pdf"
        let dest = path |> Path.changeExtension ".eps"
        Iac.saveToEPS path dir
        printf "Hello"
        pass()
  ]