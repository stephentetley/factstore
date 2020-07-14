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


    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns an error if either `o1` or `o2` are errors.
    ///
    pub def lift2(f: (t1, t2) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2]): RowWriter[u] =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns an error if any of `o1`, `o2` and `o3` are errors.
    ///
    pub def lift3(f: (t1, t2, t3) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3]): RowWriter[u] =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if any of `o1`, `o2`, `o3` and `o4` are errors.
    ///
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4]): RowWriter[u] =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o5` are errors.
    ///
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5]): RowWriter[u] =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o6` are errors.
    ///
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5], o6: RowWriter[t6]): RowWriter[u] =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o7` are errors.
    ///
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5], o6: RowWriter[t6], o7: RowWriter[t7]): RowWriter[u] =
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o8` are errors.
    ///
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5], o6: RowWriter[t6], o7: RowWriter[t7], o8: RowWriter[t8]): RowWriter[u] =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o9` are errors.
    ///
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5], o6: RowWriter[t6], o7: RowWriter[t7], o8: RowWriter[t8], o9: RowWriter[t9]): RowWriter[u] =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o10` are errors.
    ///
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef, o1: RowWriter[t1], o2: RowWriter[t2], o3: RowWriter[t3], o4: RowWriter[t4], o5: RowWriter[t5], o6: RowWriter[t6], o7: RowWriter[t7], o8: RowWriter[t8], o9: RowWriter[t9], o10: RowWriter[t10]): RowWriter[u] =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)


}

