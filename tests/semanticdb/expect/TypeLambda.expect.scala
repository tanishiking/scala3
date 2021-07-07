package example

type T1/*<-example::TypeLambda$package.T1#*/ = [X/*<-example::TypeLambda$package.T1#[X]*/] =>> X/*->example::TypeLambda$package.T1#[X]*/
type T2/*<-example::TypeLambda$package.T2#*/ = [X/*<-example::TypeLambda$package.T2#[X]*/] =>> List/*->scala::package.List#*/[X/*->example::TypeLambda$package.T2#[X]*/]
type T3/*<-example::TypeLambda$package.T3#*/ = [X/*<-example::TypeLambda$package.T3#[X]*/] =>> Map/*->scala::Predef.Map#*/[String/*->scala::Predef.String#*/, X/*->example::TypeLambda$package.T3#[X]*/]
type T4/*<-example::TypeLambda$package.T4#*/ = [X/*<-example::TypeLambda$package.T4#[X]*/, Y/*<-example::TypeLambda$package.T4#[Y]*/] =>> Map/*->scala::Predef.Map#*/[Y/*->example::TypeLambda$package.T4#[Y]*/, X/*->example::TypeLambda$package.T4#[X]*/]

type B1/*<-example::TypeLambda$package.B1#*/[X/*<-example::TypeLambda$package.B1#[X]*/ <: String/*->scala::Predef.String#*/] = X/*->example::TypeLambda$package.B1#[X]*/
type B2/*<-example::TypeLambda$package.B2#*/[X/*<-example::TypeLambda$package.B2#[X]*/ >: Int/*->scala::Int#*/] = Any/*->scala::Any#*/

type V1/*<-example::TypeLambda$package.V1#*/[+X/*<-example::TypeLambda$package.V1#[X]*/ <: X/*->example::TypeLambda$package.V1#[X]*/ => X/*->example::TypeLambda$package.V1#[X]*/] = Any/*->scala::Any#*/

type AList/*<-example::TypeLambda$package.AList#*/[X/*<-example::TypeLambda$package.AList#[X]*/] = List/*->scala::package.List#*/[X/*->example::TypeLambda$package.AList#[X]*/] // AList = [X] =>> List[X]

type Union/*<-example::TypeLambda$package.Union#*/ = [A/*<-example::TypeLambda$package.Union#[A]*/] =>> [B/*<-example::TypeLambda$package.Union#[B]*/] =>> [C/*<-example::TypeLambda$package.Union#[C]*/] =>> A/*->example::TypeLambda$package.Union#[A]*/ |/*->scala::`|`#*/ B/*->example::TypeLambda$package.Union#[B]*/ |/*->scala::`|`#*/ C/*->example::TypeLambda$package.Union#[C]*/
type MapKV/*<-example::TypeLambda$package.MapKV#*/ = [K/*<-example::TypeLambda$package.MapKV#[K]*/] =>> [V/*<-example::TypeLambda$package.MapKV#[V]*/] =>> Map/*->scala::Predef.Map#*/[K/*->example::TypeLambda$package.MapKV#[K]*/, V/*->example::TypeLambda$package.MapKV#[V]*/]

type Tuple/*<-example::TypeLambda$package.Tuple#*/ = [X/*<-example::TypeLambda$package.Tuple#[X]*/] =>> [Y/*<-example::TypeLambda$package.Tuple#[Y]*/] =>> (X/*->example::TypeLambda$package.Tuple#[X]*/, Y/*->example::TypeLambda$package.Tuple#[Y]*/)
type TupleIntString/*<-example::TypeLambda$package.TupleIntString#*/ = Tuple/*->example::TypeLambda$package.Tuple#*/[Int/*->scala::Int#*/][String/*->scala::Predef.String#*/]

