

type Elem/*<-_empty_::MatchType$package.Elem#*/[X/*<-_empty_::MatchType$package.Elem#[X]*/] <: Any/*->scala::Any#*/ = X/*->_empty_::MatchType$package.Elem#[X]*/ match
  case String/*->scala::Predef.String#*/ => Char/*->scala::Char#*/
  case Array/*->scala::Array#*/[t/*<-local0*/] => t/*->local0*/

// type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match
//   case EmptyTuple => Ys
//   case x *: xs => x *: Concat[xs, Ys]

