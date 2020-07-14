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

use Factstore.RowWriter;
use Factstore.RowWriter.{RowWriter};

use Factstore/Internal.PreparedStatement;
use Factstore/Internal.PreparedStatement.{PreparedStatement};

// Big design issue - hidden incrementing parameter index or an explicit one?

namespace Factstore {

    pub opaque type RowWriter[a] = PreparedStatement ~> Result[a, System.Error]

}

namespace Factstore/RowWriter {

    pub def return(x: a): RowWriter[a] =
        RowWriter(_ -> Ok(x) as & Impure)

    def apply1(x: RowWriter[a], r: PreparedStatement): Result[a, System.Error] & Impure =
        let RowWriter(f) = x;
        f(r)

    pub def map(f: a -> b & ef, x: RowWriter[a]): RowWriter[b] =
        RowWriter(r ->
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )

    pub def ap(mf: RowWriter[a -> b & ef], ma: RowWriter[a]): RowWriter[b] =
        RowWriter(r ->
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

    pub def bind(x: RowWriter[a], k: a -> RowWriter[b]): RowWriter[b] =
        RowWriter(r ->
            match apply1(x, r) {
                case Ok(a) => { let ans = k(a); apply1(ans, r) }
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> RowWriter[b], x: RowWriter[a]): RowWriter[b] = bind(x, k)

    def liftPrim(update: PreparedStatement ~> Result[a, System.Error]): RowWriter[a] =
        RowWriter(r -> update(r))


    pub def tellString!(ix: Int32, str: String): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setString!(x, ix, str))

    pub def tellBool!(ix: Int32, b: Bool): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setBool!(x, ix, b))

    pub def tellInt8!(ix: Int32, n: Int8): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setInt8!(x, ix, n))

    pub def tellInt16!(ix: Int32, n: Int16): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setInt16!(x, ix, n))

    pub def tellInt32!(ix: Int32, n: Int32): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setInt32!(x, ix, n))

    pub def tellInt64!(ix: Int32, n: Int64): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setInt64!(x, ix, n))

    pub def tellFloat32!(ix: Int32, n: Float32): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setFloat32!(x, ix, n))

    pub def tellFloat64!(ix: Int32, n: Float64): RowWriter[Unit] =
        liftPrim(x -> Factstore/Internal/PreparedStatement.setFloat64!(x, ix, n))

}

