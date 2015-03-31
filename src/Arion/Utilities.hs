module Arion.Utilities (
    associate,
    dependencies
) where

import Arion.Types
import Data.Map (Map, fromList)
import Data.List (union)

associate :: [SourceFile] -> [TestFile] -> Map FilePath [TestFile]
associate sourceFiles testFiles = let sourcesAndDependencies = dependencies sourceFiles
                                  in fromList $ map (\(source, dependencies) ->
                                                                    let testFilesFor source = filter (\testFile -> moduleName source `elem` imports testFile) testFiles 
                                                                        testFilesForSource = testFilesFor source
                                                                        testFilesForDependencies = concatMap testFilesFor dependencies
                                                                    in (sourceFilePath source, testFilesForSource ++ testFilesForDependencies)
                                                    ) sourcesAndDependencies

dependencies :: [SourceFile] -> [(SourceFile, [SourceFile])]
dependencies [] = []
dependencies sourceFiles = map (\file -> let dependencies = transitiveDependencies sourceFiles (importedModules file)
                                         in (file, dependencies)
                           ) sourceFiles

transitiveDependencies :: [SourceFile] -> [String] -> [SourceFile]
transitiveDependencies sourceFiles [] = []
transitiveDependencies sourceFiles imports = let sourcesForImports = concatMap (findSourcesByModule sourceFiles) imports
                                             in sourcesForImports ++ concatMap (transitiveDependencies sourceFiles) (map importedModules sourcesForImports)

findSourcesByModule :: [SourceFile] -> String -> [SourceFile]
findSourcesByModule sourceFiles theModuleName = filter (\file -> moduleName file == theModuleName) sourceFiles
