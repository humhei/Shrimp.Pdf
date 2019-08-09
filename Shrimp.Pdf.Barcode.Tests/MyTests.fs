module MyTests

open Expecto
open Shrimp.Pdf
let pass() = Expect.isTrue true "passed"
let fail() = Expect.isTrue false "failed"
let MyTests =
  testList "MyTests" [
    testCase "valid ean13 test 1" <| fun _ -> 
        Expect.isFalse (Barcode.isValidEan13 "7453089535059") "passed"
    testCase "valid ean13 test 2" <| fun _ -> 
        Expect.isTrue (Barcode.isValidEan13 "7453089535070") "passed"
    testCase "valid ean13 test 3" <| fun _ -> 
        Expect.isTrue (Barcode.isValidEan13 "7453089535063") "passed"
  ]