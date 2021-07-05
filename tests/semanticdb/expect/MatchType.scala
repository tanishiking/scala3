

type Elem[X] <: Any = X match
  case String => Char
  case Array[t] => t

// type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match
//   case EmptyTuple => Ys
//   case x *: xs => x *: Concat[xs, Ys]

