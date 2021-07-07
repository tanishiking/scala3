package example

type T1 = [X] =>> X
type T2 = [X] =>> List[X]
type T3 = [X] =>> Map[String, X]
type T4 = [X, Y] =>> Map[Y, X]

type B1[X <: String] = X
type B2[X >: Int] = Any

type V1[+X <: X => X] = Any

type AList[X] = List[X] // AList = [X] =>> List[X]

type Union = [A] =>> [B] =>> [C] =>> A | B | C
type MapKV = [K] =>> [V] =>> Map[K, V]

type Tuple = [X] =>> [Y] =>> (X, Y)
type TupleIntString = Tuple[Int][String]

