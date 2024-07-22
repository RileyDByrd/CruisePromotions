package byrd.riley.cruisepromotions

@main
def main(): Unit =

  val promotions: Seq[Promotion] = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  println("All Promotion Combinations")
  println(allCombinablePromotions(promotions).mkString("\n"))

  println("\nPromotion Combinations for promotionCode=\"P1\"")
  println(combinablePromotions("P1", promotions).mkString("\n"))

  println("\nPromotion Combinations for promotionCode=\"P3\"")
  println(combinablePromotions("P3", promotions).mkString("\n"))
