use Factstore/Export.{makeConnectString};


def main(): Result[Bool, System.Error] & Impure =
    use Result.flatMap;
    use Factstore/Internal/Factstore.{new, execute, close};
    let path = "e:/coding/flix/factstore/output/db1.sqlite";
    let url = makeConnectString(path);
    let* db = new(url);
    let sql = "CREATE TABLE IF NOT EXISTS names (name text PRIMARY KEY NOT NULL);";
    let* _ = db.execute(sql);
    let* _ = db.close();
    Ok(true)