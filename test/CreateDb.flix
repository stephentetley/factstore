use Factstore.SQLiteExport;
use Factstore.RowWriter;
use Factstore/Export.{runSQLiteExport};

pub def apply_(funs: List[a -> Result[Unit, e] & ef] , x: a): Result[Unit, e] & ef = match funs { 
    case fn :: rs => match fn(x) {
        case Ok(_) => apply_(rs, x)
        case Err(e) => Err(e)
    }
    case Nil => Ok() 
}

rel Dbms(name: String, year: Int32)

def databases(): #{Dbms} = #{
    Dbms("PostgreSQL", 1996).
    Dbms("SQLite", 2000).
    Dbms("DuckDB", 2019).
}

def writeOutput(): SQLiteExport[Unit] =
    use Factstore/Export.{flatMap, return, execute, emptySQLiteWriteList, execWriteList, setAutoCommit, commit, close};
    let* _          = execute("CREATE TABLE IF NOT EXISTS dbms (name text PRIMARY KEY NOT NULL, year int);");
    let* _          = execute("DELETE FROM dbms;");
    let stmt        = "INSERT INTO dbms(name,capacity) VALUES(?,?)";
    let* _          = setAutoCommit(false);
    let writeStep   = Factstore/Export.makeSQLiteWriteStep(writeCells);
    let writes      = { fold Dbms (emptySQLiteWriteList()) writeStep databases() };
    let* _          = execWriteList(stmt, writes);
    let* _          = commit();
    let* _          = close();
    return()


/// A "cell printer" for the body of Dbms.
def writeCells(src: (String,Int)): RowWriter[Unit] = 
    use Factstore/RowWriter.{flatMap, return, tellString!, tellInt32!};
    let (name,year) = src;
    let* _ = tellString!(0, name);
    let* _ = tellInt32!(0, year);
    return()


def main(): Result[Bool, System.Error] & Impure =
    use Result.flatMap;
    let str = "e:/coding/flix/factstore/output/db1.sqlite";
    let path = System/FilePath.new(str);
    let* _ = System/File.deleteIfExists(path);
    let* _ = runSQLiteExport(writeOutput(), path);
    Ok(true)


