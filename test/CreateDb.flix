use Factstore.SQLiteExport;
use Factstore.SQLiteWriteList;
use Factstore.RowWriter;


rel Dbms(name: String, year: Int32)

def databases(): #{Dbms} = #{
    Dbms("PostgreSQL", 1996).
    Dbms("SQLite", 2000).
    Dbms("MonetDB", 2002).
    Dbms("DuckDB", 2019).
}

/// Weird - making this a top level definition (with a type signature) seems to stop the program crashing.
def writeStep(): ((String, Int32), SQLiteWriteList) -> SQLiteWriteList = Factstore/Export.makeSQLiteWriteStep(writeCells)


def writeOutput(): SQLiteExport[Unit] =
    use Factstore/Export.{flatMap, return, execute, emptySQLiteWriteList, execWriteList, setAutoCommit, commit, close};
    let* _          = execute("CREATE TABLE IF NOT EXISTS dbms (name text PRIMARY KEY NOT NULL, year int);");
    let* _          = execute("DELETE FROM dbms;");
    let stmt        = "INSERT INTO dbms(name, year) VALUES(?,?)";
    let* _          = setAutoCommit(false);
    let writes      = fold Dbms emptySQLiteWriteList() writeStep() (solve databases());
    let* _          = execWriteList(stmt, writes);
    let* _          = commit();
    let* _          = close();
    return()


/// A "cell printer" for the body of Dbms.
def writeCells(src: (String, Int32)): RowWriter[Unit] = 
    use Factstore/RowWriter.{flatMap, return, tellString!, tellInt32!};
    let (name,year) = src;
    let* _ = tellString!(1, name);
    let* _ = tellInt32!(2, year);
    return()


def main(): Result[Bool, System.Error] & Impure =
    use Result.flatMap;
    use Factstore/Export.{runSQLiteExport};
    let str = "e:/coding/flix/factstore/output/db1.sqlite";
    let path = System/FilePath.new(str);    
    let* _ = System/File.deleteIfExists(path);
    let* _ = runSQLiteExport(writeOutput(), path);
    Ok(true)

// def main(): Int32 = fold Dbms 0 mkWriteStep() (solve databases())