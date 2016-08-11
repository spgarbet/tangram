testSuite <- defineTestSuite("tableGrammar",
                             dirs = "tableGrammar/unitTests",
                             testFileRegexp = "^test.+\\.R",
                             testFuncRegexp = "^test.+")

testResult <- runTestSuite(testSuite)

printTextProtocol(testResult)