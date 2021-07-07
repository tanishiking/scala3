package matchtype

def matchRes/*<-matchtype::MatchType$package.matchRes().*/[T/*<-matchtype::MatchType$package.matchRes().[T]*/](x/*<-matchtype::MatchType$package.matchRes().(x)*/: T/*->matchtype::MatchType$package.matchRes().[T]*/): T/*->matchtype::MatchType$package.matchRes().[T]*/ match { case String/*->scala::Predef.String#*/ => Char/*->scala::Char#*/ } = ???/*->scala::Predef.`???`().*/

type Elem/*<-matchtype::MatchType$package.Elem#*/[X/*<-matchtype::MatchType$package.Elem#[X]*/] <: Any/*->scala::Any#*/ = X/*->matchtype::MatchType$package.Elem#[X]*/ match
  case String/*->scala::Predef.String#*/ => Char/*->scala::Char#*/
  case Array/*->scala::Array#*/[t/*<-local0*/] => t/*->local0*/

type Concat/*<-matchtype::MatchType$package.Concat#*/[Xs/*<-matchtype::MatchType$package.Concat#[Xs]*/ <: Tuple/*->scala::Tuple#*/, +Ys/*<-matchtype::MatchType$package.Concat#[Ys]*/ <: Tuple/*->scala::Tuple#*/] <: Tuple/*->scala::Tuple#*/ = Xs/*->matchtype::MatchType$package.Concat#[Xs]*/ match
  case EmptyTuple/*->scala::Tuple$package.EmptyTuple#*/ => Ys/*->matchtype::MatchType$package.Concat#[Ys]*/
  case x/*<-local1*/ *:/*->scala::`*:`#*/ xs/*<-local2*/ => x/*->local1*/ *:/*->scala::`*:`#*/ Concat/*->matchtype::MatchType$package.Concat#*/[xs/*->local2*/, Ys/*->matchtype::MatchType$package.Concat#[Ys]*/]

