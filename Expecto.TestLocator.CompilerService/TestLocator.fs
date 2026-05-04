namespace Expecto.TestLocator.CompilerService

module internal ASTTestLocator =
    open FSharp.Compiler.Text
    open FSharp.Compiler.Syntax
    open System

    type TestAdapterEntry<'range> = { 
        Name: string
        Range: 'range
        Childs: ResizeArray<TestAdapterEntry<'range>>
        Id: int
        List: bool
        ModuleType: string
        Type: string 
    }
    
    [<Literal>]
    let private NoneModuleType = "NoneModule"
    [<Literal>]
    let private ExpectoType = "Expecto"

    let rec private (|Sequentials|_|) =
        function
        | SynExpr.Sequential(expr1 = e; expr2 = Sequentials es) -> Some(e :: es)
        | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> Some [ e1; e2 ]
        | _ -> None


    let getTestHierarchyFromAST (ast: ParsedInput) : TestAdapterEntry<range> list =
        let mutable ident = 0

        let isExpectoName (str: string) =
            str.EndsWith("testCase", StringComparison.Ordinal)
            || str.EndsWith("ftestCase", StringComparison.Ordinal)
            || str.EndsWith("ptestCase", StringComparison.Ordinal)
            || str.EndsWith("testCaseAsync", StringComparison.Ordinal)
            || str.EndsWith("ftestCaseAsync", StringComparison.Ordinal)
            || str.EndsWith("ptestCaseAsync", StringComparison.Ordinal)
            || str.EndsWith("testCaseTask", StringComparison.Ordinal)
            || str.EndsWith("ftestCaseTask", StringComparison.Ordinal)
            || str.EndsWith("ptestCaseTask", StringComparison.Ordinal)
            || (str.EndsWith("test", StringComparison.Ordinal)
                && not (str.EndsWith("failtest", StringComparison.Ordinal))
                && not (str.EndsWith("skiptest", StringComparison.Ordinal)))
            || str.EndsWith("ftest", StringComparison.Ordinal)
            || (str.EndsWith("ptest", StringComparison.Ordinal)
                && not (str.EndsWith("skiptest", StringComparison.Ordinal)))
            || str.EndsWith("testAsync", StringComparison.Ordinal)
            || str.EndsWith("ftestAsync", StringComparison.Ordinal)
            || str.EndsWith("ptestAsync", StringComparison.Ordinal)
            || str.EndsWith("testTask", StringComparison.Ordinal)
            || str.EndsWith("ftestTask", StringComparison.Ordinal)
            || str.EndsWith("ptestTask", StringComparison.Ordinal)
            || str.EndsWith("testProperty", StringComparison.Ordinal)
            || str.EndsWith("ptestProperty", StringComparison.Ordinal)
            || str.EndsWith("ftestProperty", StringComparison.Ordinal)
            || str.EndsWith("testPropertyWithConfig", StringComparison.Ordinal)
            || str.EndsWith("ptestPropertyWithConfig", StringComparison.Ordinal)
            || str.EndsWith("ftestPropertyWithConfig", StringComparison.Ordinal)
            || str.EndsWith("testPropertyWithConfigs", StringComparison.Ordinal)
            || str.EndsWith("ptestPropertyWithConfigs", StringComparison.Ordinal)
            || str.EndsWith("ftestPropertyWithConfigs", StringComparison.Ordinal)
            || str.EndsWith("testTheory", StringComparison.Ordinal)
            || str.EndsWith("ftestTheory", StringComparison.Ordinal)
            || str.EndsWith("ptestTheory", StringComparison.Ordinal)
            || str.EndsWith("testTheoryAsync", StringComparison.Ordinal)
            || str.EndsWith("ftestTheoryAsync", StringComparison.Ordinal)
            || str.EndsWith("ptestTheoryAsync", StringComparison.Ordinal)
            || str.EndsWith("testTheoryTask", StringComparison.Ordinal)
            || str.EndsWith("ftestTheoryTask", StringComparison.Ordinal)
            || str.EndsWith("ptestTheoryTask", StringComparison.Ordinal)

        let isExpectoListName (str: string) =
            str.EndsWith("testList", StringComparison.Ordinal)
            || str.EndsWith("ftestList", StringComparison.Ordinal)
            || str.EndsWith("ptestList", StringComparison.Ordinal)

        let (|Case|List|NotExpecto|) =
            function
            | SynExpr.Ident i ->
                if isExpectoName i.idText then Case
                elif isExpectoListName i.idText then List
                else NotExpecto
            | SynExpr.LongIdent(_, SynLongIdent(id = lst), _, _) ->
                let i = lst |> List.last

                if isExpectoName i.idText then Case
                elif isExpectoListName i.idText then List
                else NotExpecto
            | _ -> NotExpecto

        let (|FindTestCases|_|) expr =
            match expr with
            | SynExpr.App(
                funcExpr = SynExpr.App(
                funcExpr = SynExpr.App(funcExpr = expr1; argExpr = SynExpr.Const(constant = SynConst.String(text = s)))))
            | SynExpr.App(
                argExpr = SynExpr.App(
                funcExpr = SynExpr.App(funcExpr = expr1; argExpr = SynExpr.Const(constant = SynConst.String(text = s)))))
            | SynExpr.App(
                funcExpr = SynExpr.App(funcExpr = expr1); argExpr = SynExpr.Const(constant = SynConst.String(text = s)))
            | SynExpr.App(
                funcExpr = SynExpr.App(funcExpr = SynExpr.App(funcExpr = expr1))
                argExpr = SynExpr.Const(constant = SynConst.String(text = s)))
            | SynExpr.App(
                funcExpr = SynExpr.App(funcExpr = SynExpr.App(funcExpr = SynExpr.App(funcExpr = expr1)))
                argExpr = SynExpr.Const(constant = SynConst.String(text = s)))
            | SynExpr.App(funcExpr = expr1; argExpr = SynExpr.Const(constant = SynConst.String(text = s))) -> Some(expr1, s)
            | _ -> None

        let rec visitExpr (parent: TestAdapterEntry<range>) =
            function
            | SynExpr.App(_, _, SynExpr.App(_, _, expr1, SynExpr.Const(SynConst.String(text = s), _), range), expr2, _) ->
                match expr1, expr2 with
                | List, SynExpr.ArrayOrList _
                | List, SynExpr.ArrayOrListComputed _ ->
                    ident <- ident + 1

                    let entry = { 
                        Name = s
                        Range = range
                        Childs = ResizeArray()
                        Id = ident
                        List = true
                        ModuleType = NoneModuleType
                        Type = ExpectoType 
                    }

                    parent.Childs.Add entry

                    visitExpr entry expr1
                    visitExpr entry expr2
                | Case, SynExpr.ComputationExpr _
                | Case, SynExpr.Lambda _
                | Case, SynExpr.Paren(expr = SynExpr.App(argExpr = SynExpr.ComputationExpr _))
                | Case, SynExpr.Paren(expr = (SynExpr.Lambda _)) ->
                    ident <- ident + 1

                    let entry = { 
                        Name = s
                        Range = expr1.Range
                        Childs = ResizeArray()
                        Id = ident
                        List = false
                        ModuleType = NoneModuleType
                        Type = ExpectoType 
                    }

                    parent.Childs.Add entry
                | _ ->
                    visitExpr parent expr1
                    visitExpr parent expr2
            | FindTestCases(expr1, s) -> //Take those applications that are using string constant as an argument
                match expr1 with
                | Case ->
                    ident <- ident + 1

                    let entry = { 
                        Name = s
                        Range = expr1.Range
                        Childs = ResizeArray()
                        Id = ident
                        List = false
                        ModuleType = NoneModuleType
                        Type = ExpectoType 
                    }

                    parent.Childs.Add entry
                | List -> ()
                | NotExpecto -> ()
            | SynExpr.ArrayOrListComputed(_, expr, _)
            | SynExpr.ComputationExpr(expr = expr)
            | SynExpr.Lambda(body = expr)
            | SynExpr.YieldOrReturn(expr = expr)
            | SynExpr.YieldOrReturnFrom(expr = expr)
            | SynExpr.New(_, _, expr, _)
            | SynExpr.Assert(expr, _)
            | SynExpr.Do(expr, _)
            | SynExpr.Typed(expr, _, _)
            | SynExpr.Paren(expr, _, _, _)
            | SynExpr.DoBang(expr = expr)
            | SynExpr.Downcast(expr, _, _)
            | SynExpr.For(doBody = expr)
            | SynExpr.Lazy(expr, _)
            | SynExpr.TypeTest(expr, _, _)
            | SynExpr.Upcast(expr, _, _)
            | SynExpr.InferredUpcast(expr, _)
            | SynExpr.InferredDowncast(expr, _)
            | SynExpr.LongIdentSet(_, expr, _)
            | SynExpr.DotGet(expr, _, _, _)
            | SynExpr.ForEach(bodyExpr = expr) -> visitExpr parent expr
            | SynExpr.App(_, _, expr1, expr2, _)
            | SynExpr.TryFinally(tryExpr = expr1; finallyExpr = expr2)
            | SynExpr.NamedIndexedPropertySet(_, expr1, expr2, _)
            | SynExpr.DotNamedIndexedPropertySet(_, _, expr1, expr2, _)
            | SynExpr.LetOrUseBang(rhs = expr1; body = expr2)
            | SynExpr.While(_, expr1, expr2, _) ->
                visitExpr parent expr1
                visitExpr parent expr2
            | Sequentials exprs
            | SynExpr.Tuple(_, exprs, _, _)
            | SynExpr.ArrayOrList(_, exprs, _) -> List.iter (visitExpr parent) exprs
            | SynExpr.Match(expr = expr; clauses = clauses)
            | SynExpr.TryWith(tryExpr = expr; withCases = clauses) ->
                visitExpr parent expr
                visitMatches parent clauses
            | SynExpr.IfThenElse(ifExpr = cond; thenExpr = trueBranch; elseExpr = falseBranchOpt) ->
                visitExpr parent cond
                visitExpr parent trueBranch
                falseBranchOpt |> Option.iter (visitExpr parent)
            | SynExpr.LetOrUse(bindings = bindings; body = body) ->
                visitBindings parent bindings
                visitExpr parent body
            | SynExpr.Record(_, _, fields, _) ->
                fields
                |> List.choose (fun (SynExprRecordField(expr = expr)) -> expr)
                |> List.iter (visitExpr parent)
            | SynExpr.MatchLambda(_, _, clauses, _, _) -> visitMatches parent clauses
            | SynExpr.ObjExpr(bindings = bindings) -> visitBindings parent bindings
            | _ -> ()

        and visitBinding prefix (SynBinding(expr = body)) = visitExpr prefix body
        and visitBindings prefix s = s |> List.iter (visitBinding prefix)
        and visitMatch prefix (SynMatchClause(resultExpr = expr)) = visitExpr prefix expr
        and visitMatches prefix s = s |> List.iter (visitMatch prefix)

        let rec visitDeclarations prefix decls =
            for declaration in decls do
                match declaration with
                | SynModuleDecl.Let(_, bindings, _) -> visitBindings prefix bindings
                | SynModuleDecl.NestedModule(decls = decls) -> visitDeclarations prefix decls
                | _ -> ()

        let visitModulesAndNamespaces prefix modulesOrNss =
            Seq.iter (fun (SynModuleOrNamespace(decls = decls)) -> visitDeclarations prefix decls) modulesOrNss

        let allTests = { 
            Name = ""
            Range = Range.range0
            Childs = ResizeArray()
            Id = -1
            List = false
            ModuleType = NoneModuleType
            Type = "" 
        }

        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) -> visitModulesAndNamespaces allTests modules
        | _ -> ()

        List.ofSeq allTests.Childs

    open FSharp.Compiler.CodeAnalysis

    type ASTSourceLocation = { 
        testNameSegments: string list
        sourceFilePath: string
        lineNumber: int 
    }
    

    let adapterEntryToSourceLocations (filePath: string) (adapterEntry: TestAdapterEntry<range>) : ASTSourceLocation list =
        let rec recurse (parentNameSegments: string list) (adapterEntry: TestAdapterEntry<range>) : ASTSourceLocation list = 
            let nameSegments = List.append parentNameSegments [adapterEntry.Name]
            
            let location = 
                {
                    testNameSegments = nameSegments
                    sourceFilePath = filePath
                    lineNumber = adapterEntry.Range.StartLine
                }

            let childLocations = adapterEntry.Childs |> List.ofSeq |> List.collect (recurse nameSegments)

            location :: childLocations

        recurse [] adapterEntry
            
    open System.IO
    let getTestsLocationsFromProject (projectPath: string) : Async<ASTSourceLocation list> = 
        let getTestLocationsForFile (parseResult: FSharpParseFileResults) =
            let adapterEntries = getTestHierarchyFromAST parseResult.ParseTree
            let locations = adapterEntries |> List.collect (adapterEntryToSourceLocations parseResult.FileName)
            locations 
        
        async {
            let checker = FSharpChecker.Create()
            let projectDirectory = Path.GetDirectoryName(projectPath)
            let files = System.IO.Directory.GetFiles(projectDirectory, "*.fs") |> List.ofArray
            let parseIndividual (filePath: string) = 
                let source=  File.ReadAllText(filePath)
                checker.ParseFile(filePath, SourceText.ofString source, {FSharpParsingOptions.Default with SourceFiles= [|filePath|]})
            let! untypedASTByFile = files |> List.map parseIndividual |> Async.Sequential

            let tests = untypedASTByFile |> List.ofArray |> List.collect getTestLocationsForFile
            return tests
        }

module Stateful = 
    let mutable blah = 5

module TestLocator =
    open Expecto
    // now I need to reference the expecto assembly
    let getTestNameToLocationMap (projectFilePath: string) : Map<string list, SourceLocation> Async= 
        // NOTE: Test names are expected to be unique per-project. This can technically be turned off for expecto's console runner, but is manadatory for any test explorer 
        async {
            let! locations = ASTTestLocator.getTestsLocationsFromProject projectFilePath
            
            return 
                locations |> List.map (fun l -> 
                    (l.testNameSegments, { sourcePath = l.sourceFilePath; lineNumber = l.lineNumber })
                )
                |> Map
        }

