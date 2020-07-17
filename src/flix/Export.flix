/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use Data/Transient.DList;
use System.Error.{IOException, Generic};
use Text.Charset.{Charset};

use Factstore.SQLiteExport;
use Factstore.SQLiteExport.{SQLiteExport};
use Factstore.SQLiteWriteList;
use Factstore.SQLiteWriteList.{SQLiteWriteList};

use Factstore.RowWriter;
use Factstore.RowWriter.{RowWriter};
use Factstore/Internal.PreparedStatement;


namespace Factstore {

    pub opaque type SQLiteExport[a] = Factstore/Internal.Factstore ~> Result[a, System.Error]

    /// CsvWriteList is a workaround because fixpoint fold is our only way of
    /// extracting data from a relation and the step function of fixpoint fold
    /// must be pure - solution we build a list of impure "write" functions
    /// to be invoked after the fold.
    pub opaque type SQLiteWriteList = DList[RowWriter[Unit]]

}

namespace Factstore/Export {

    def makeConnectString(dbpath: String): String = "jdbc:sqlite:" + dbpath

    pub def runSQLiteExport(mx: SQLiteExport[a], dbpath: System.FilePath): Result[a, System.Error] & Impure =
        use Result.flatMap;
        use System/FilePath.{normalize, toString};
        use Factstore/Internal/Factstore.{new => newFactstore};
        let url = makeConnectString(dbpath.normalize().toString());
        let* store = newFactstore(url);
        apply1(mx, store)



    pub def return(x: a): SQLiteExport[a] =
        SQLiteExport(_ -> Ok(x) as & Impure)

    def apply1(x: SQLiteExport[a], r: Factstore/Internal.Factstore): Result[a, System.Error] & Impure =
        let SQLiteExport(f) = x;
        f(r)

    pub def map(f: a -> b & ef, x: SQLiteExport[a]): SQLiteExport[b] =
        SQLiteExport(r ->
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )

    pub def ap(mf: SQLiteExport[a -> b & ef], ma: SQLiteExport[a]): SQLiteExport[b] =
        SQLiteExport(r ->
            match apply1(mf, r) {
                case Ok(f) => {
                    match apply1(ma, r) {
                        case Ok(a) => Ok(f(a) as & Impure)
                        case Err(e2) => Err(e2)
                    }
                }
                case Err(e1) => Err(e1)
            }
        )


    pub def bind(x: SQLiteExport[a], k: a -> SQLiteExport[b]): SQLiteExport[b] =
        SQLiteExport(r ->
            match apply1(x, r) {
                case Ok(a) => { let ans = k(a); apply1(ans, r) }
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> SQLiteExport[b], x: SQLiteExport[a]): SQLiteExport[b] = bind(x, k)

    pub def liftAction(fn: Unit ~> Result[a, System.Error]): SQLiteExport[a] =
        SQLiteExport(_ -> fn())


    def liftPrim(prim: Factstore/Internal.Factstore ~> Result[a, System.Error]): SQLiteExport[a] =
        SQLiteExport(r -> prim(r))

    pub def close(): SQLiteExport[Unit] =
        use Factstore/Internal/Factstore.{close};
        liftPrim(x -> x.close())

    pub def setAutoCommit(autoCommit: Bool): SQLiteExport[Unit] =
        use Factstore/Internal/Factstore.{setAutoCommit};
        liftPrim(x -> x.setAutoCommit(autoCommit))

    pub def commit(): SQLiteExport[Unit] =
        use Factstore/Internal/Factstore.{commit};
        liftPrim(x -> x.commit())

    pub def execute(sql: String): SQLiteExport[Bool] =
        use Factstore/Internal/Factstore.{execute};
        liftPrim(x -> x.execute(sql))

    /// Don't export...
    def prepareStatement(sql: String): SQLiteExport[PreparedStatement] =
        use Factstore/Internal/Factstore.{prepareStatement};
        liftPrim(x -> x.prepareStatement(sql))

    pub def emptySQLiteWriteList(): SQLiteWriteList =
        use Data/Transient/DList.{empty};
        SQLiteWriteList(empty())

    pub def makeSQLiteWriteStep(f: x -> RowWriter[Unit]): (x, SQLiteWriteList) -> SQLiteWriteList =
        (x, ws) ->
            let SQLiteWriteList(ws1) = ws;
            let w1 = f(x);
            SQLiteWriteList(Data/Transient/DList.snoc(ws1, w1))

    pub def execWriteList(sql: String, xs: SQLiteWriteList): SQLiteExport[Unit] =
        let SQLiteWriteList(xs1) = xs;
        let writes = Data/Transient/DList.toList(xs1);
        let* stmt = prepareStatement(sql);
        execWriteListHelper(stmt, writes)

    def execWriteListHelper(stmt: PreparedStatement, xs: List[RowWriter[Unit]]): SQLiteExport[Unit] = match xs {
        case Nil => return()
        case mx :: rs => {
            let* _ = execWriteList1(stmt, mx);
            execWriteListHelper(stmt, rs)
        }
    }

    def execWriteList1(stmt: PreparedStatement, mx: RowWriter[a]): SQLiteExport[Unit] =
        let RowWriter(mf) = mx;
        let* _ = liftAction(_ -> Factstore/Internal/PreparedStatement.clearParameters!(stmt));
        let* _ = liftAction(_ -> mf(stmt));
        let* _ = liftAction(_ -> Factstore/Internal/PreparedStatement.executeUpdate(stmt));
        return()



 }