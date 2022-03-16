namespace FSharp.ObjectCatalogViews

open Xunit
open Xunit.Abstractions

open System.IO
open System.Reflection
open FSharp.xUnit

type LakeDatabaseTest(output: ITestOutputHelper) =
    let locatePath = Path.Combine(DirectoryInfo(__SOURCE_DIRECTORY__).Parent.FullName,"Databases")
    let ass = typeof<Lake.ASME.Elbow>.Assembly

    [<Fact(Skip="done!")>]
    member this.``generate Lake Test``() =
        let db_name = "Lake"
        let code = ReadOnlyRecord.databaseDefinition ConnectionString.connstr db_name

        let filePath = Path.Combine(locatePath, db_name + ".fs")
        File.WriteAllText(filePath, code)

        output.WriteLine("lake done!")
        output.WriteLine(filePath)

    [<Fact>]
    member this.``valid schema Test``() =
        let db_name = "Lake"
        let schemas = TableMeta.getStructuralSchemas ConnectionString.connstr db_name

        let fromDb =
            schemas
            |> Array.map(fun sch ->
                sch.tables
                |> Array.map(fun table ->
                    table.columns
                    |> Array.map(fun col -> col.name, SqlTypeUtils.getType col.is_nullable col.type_name)
                )
            )

        let fromFile =
            schemas
            |> Array.map(fun sch ->
                sch.tables
                |> Array.map(fun table ->
                    AssemblyReader.getRecordFields ass db_name sch.name table.name
                )
            )

        Should.equal fromDb fromFile

    [<Fact>]
    member this.``valid data Test``() =
        let db_name = "Lake"
        let schemas = 
            TableMeta.getStructuralSchemas ConnectionString.connstr db_name
            |> Array.filter(fun sch -> sch.tables.Length > 0)

        let fromDb =
            schemas
            |> Array.collect(fun sch ->
                sch.tables
                |> Array.map(fun table ->
                    let dbData = TableMeta.readTable ConnectionString.connstr db_name sch.name table
                    (sch.name, table.name), dbData
                )
            )
            |> Map.ofArray

        let fromFile =
            schemas
            |> Array.collect(fun sch ->
                sch.tables
                |> Array.map(fun table ->
                    let data = AssemblyReader.getDataFromReflection ass db_name sch.name table.name
                    (sch.name, table.name), data
                )
            )
            |> Map.ofArray

        schemas
        |> Array.iter(fun sch ->
            sch.tables
            |> Array.iter(fun tbl ->
                Should.equal fromDb.[sch.name,tbl.name] fromFile.[sch.name,tbl.name]                
            
            )
        )

