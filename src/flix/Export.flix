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

use System.Error.{IOException, Generic};
use Text.Charset.{Charset};

use Factstore.SQLiteExport;
use Factstore.SQLiteExport.{SQLiteExport};

namespace Factstore {

    pub opaque type SQLiteExport[a] = Factstore/Internal.Factstore ~> Result[a, System.Error]
}

namespace Factstore/Export {

    pub def makeConnectString(dbpath: String): String = "jdbc:sqlite:" + dbpath


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

    def liftPrim(prim: Factstore/Internal.Factstore ~> Result[a, System.Error]): SQLiteExport[a] =
        SQLiteExport(r -> prim(r))

    pub def setAutoCommit(autoCommit: Bool): SQLiteExport[Unit] =
        use Factstore/Internal/Factstore.{setAutoCommit};
        liftPrim(x -> x.setAutoCommit(autoCommit))

    pub def commit(): SQLiteExport[Unit] =
        use Factstore/Internal/Factstore.{commit};
        liftPrim(x -> x.commit())

    pub def execute(sql: String): SQLiteExport[Bool] =
        use Factstore/Internal/Factstore.{execute};
        liftPrim(x -> x.execute(sql))

    // pub writeRow(x: RowWriter[


 }