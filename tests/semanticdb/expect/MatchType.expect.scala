

type Elem/*<-_empty_::MatchType$package.Elem#*/[X/*<-_empty_::MatchType$package.Elem#[X]*/] = X/*->_empty_::MatchType$package.Elem#[X]*/ match
  case String/*->scala::Predef.String#*/ => Char/*->scala::Char#*/
  case Array/*->scala::Array#*/[t/*<-local0*/] => t/*->local0*/
  case Iterable/*->scala::package.Iterable#*/[t/*<-local1*/] => t/*->local1*/

type Concat/*<-_empty_::MatchType$package.Concat#*/[Xs/*<-_empty_::MatchType$package.Concat#[Xs]*/ <: Tuple/*->scala::Tuple#*/, +Ys/*<-_empty_::MatchType$package.Concat#[Ys]*/ <: Tuple/*->scala::Tuple#*/] <: Tuple/*->scala::Tuple#*/ = Xs/*->_empty_::MatchType$package.Concat#[Xs]*/ match
  case EmptyTuple/*->scala::Tuple$package.EmptyTuple#*/ => Ys/*->_empty_::MatchType$package.Concat#[Ys]*/
  case x/*<-local2*/ *:/*->scala::`*:`#*/ xs/*<-local3*/ => x/*->local2*/ *:/*->scala::`*:`#*/ Concat/*->_empty_::MatchType$package.Concat#*/[xs/*->local3*/, Ys/*->_empty_::MatchType$package.Concat#[Ys]*/]

