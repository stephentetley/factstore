use Factstore/Export.{makeConnectString};
use Factstore/Internal.Factstore;

pub def apply_(funs: List[a -> Result[Unit, e] & ef] , x: a): Result[Unit, e] & ef = match funs { 
    case fn :: rs => match fn(x) {
        case Ok(_) => apply_(rs, x)
        case Err(e) => Err(e)
    }
    case Nil => Ok() 
}

rel Dbms(name: String, year: Int32)

def relns(): #{Dbms} = #{
    Dbms("PostgreSQL", 1996).
    Dbms("SQLite", 2000).
    Dbms("DuckDB", 2019).
}

def setup(filepath: String): Result[Factstore, System.Error] & Impure =
    use Result.flatMap;
    use Factstore/Internal/Factstore.{new, execute};
    let path = System/FilePath.new(filepath);
    let* _ = System/File.deleteIfExists(path);
    let url = makeConnectString(filepath);
    let* db = new(url);
    let sql = "CREATE TABLE IF NOT EXISTS dbms (name text PRIMARY KEY NOT NULL, year int);";
    let* _ = db.execute(sql);
    Ok(db)

def relfoldM(): Result[Unit, System.Error] & Impure = 
    let step = (x, ac) -> {
        let fn = () -> { 
            let (name, year) = x;
            Console.printLine(name + " " + Int32.toString(year));
            Ok()
        };
        List.append(ac, (fn :: Nil))
    };
    let funs = fold Dbms Nil step (solve relns());
    apply_(funs, ())

def main(): Result[Bool, System.Error] & Impure =
    use Result.flatMap;
    use Factstore/Internal/Factstore.{execute, close};
    let str = "e:/coding/flix/factstore/output/db1.sqlite";
    let* db = setup(str);
    let* _ = relfoldM();
    let* _ = db.close();
    Ok(true)


