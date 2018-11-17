module Util

open Expecto

let testList testsFactory =
    let (name, testFactory, data) = testsFactory()
    let tests = data |> List.map testFactory
    testList name tests