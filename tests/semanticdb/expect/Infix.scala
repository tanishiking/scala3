trait SymbolTest {
  def shouldBe(right: Any): Unit
  def arg = 1
  this shouldBe (arg)
}
