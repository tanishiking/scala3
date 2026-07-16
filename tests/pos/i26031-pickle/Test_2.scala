package i26031pickle

object Test:
  val r = doIt[[A] =>> Pair[String, A]] { 42 }
