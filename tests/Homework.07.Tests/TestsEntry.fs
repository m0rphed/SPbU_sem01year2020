﻿module Homework07.TestsEntry
open Expecto

let config = { FsCheckConfig.defaultConfig with maxTest = 1000 }

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
