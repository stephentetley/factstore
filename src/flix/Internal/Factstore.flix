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

use System.Error.{Generic};

use Factstore/Internal.Factstore;
use Factstore/Internal.Factstore.{Factstore};
use Factstore/Internal.PreparedStatement;
use Factstore/Internal.PreparedStatement.{PreparedStatement};

namespace Factstore/Internal {

    pub opaque type Factstore = ##flix.runtime.spt.factstore.Factstore
}

namespace Factstore/Internal/Factstore {

    ///
    /// Returns a new Factstore.
    ///
    pub def new(url: String): Result[Factstore, System.Error] & Impure =
        import new flix.runtime.spt.factstore.Factstore(String) as newFactstore;
        try {
            let ans = Factstore(newFactstore(url));
            Ok(ans)
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.new"))
        }

    ///
    /// Close the internal connection to the DB.
    ///
    pub def close(x: Factstore): Result[Unit, System.Error] & Impure =
        import flix.runtime.spt.factstore.Factstore.close();
        try {
            let Factstore(x1) = x;
            x1.close();
            Ok()
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.close"))
        }

    pub def setAutoCommit(x: Factstore, autoCommit: Bool): Result[Unit, System.Error] & Impure =
        import flix.runtime.spt.factstore.Factstore.setAutoCommit(Bool);
        try {
            let Factstore(x1) = x;
            x1.setAutoCommit(autoCommit);
            Ok()
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.setAutoCommit"))
        }

    pub def commit(x: Factstore): Result[Unit, System.Error] & Impure =
        import flix.runtime.spt.factstore.Factstore.commit();
        try {
            let Factstore(x1) = x;
            x1.commit();
            Ok()
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.commit"))
        }

    pub def rollback(x: Factstore): Result[Unit, System.Error] & Impure =
        import flix.runtime.spt.factstore.Factstore.rollback();
        try {
            let Factstore(x1) = x;
            x1.rollback();
            Ok()
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.rollback"))
        }

    pub def prepareStatement(x: Factstore, sql: String): Result[PreparedStatement, System.Error] & Impure =
        import flix.runtime.spt.factstore.Factstore.prepareStatement(String);
        try {
            let Factstore(x1) = x;
            let ans = x1.prepareStatement(sql);
            Ok(PreparedStatement(ans))
        } catch {
            case _: ##java.sql.SQLException => Err(Generic("Factstore.prepareStatement"))
        }


}