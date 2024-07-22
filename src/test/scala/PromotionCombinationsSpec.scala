package byrd.riley.cruisepromotions

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec


class PromotionCombinationsSpec extends AnyWordSpec with Matchers:
  val examplePromotions: Seq[Promotion] = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  "allCombinablePromotions" must :
    "return the example output" when :
      "it receives the example input" in :
        allCombinablePromotions(examplePromotions) mustBe Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5")),
          PromotionCombo(Seq("P2", "P3")),
          PromotionCombo(Seq("P3", "P4", "P5"))
        )

  "combinablePromotions" must:
    "return the example output" when:
      "it receives example input P1" in:
        combinablePromotions("P1", examplePromotions) mustBe Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5"))
        )
      "it received example input P3" in:
        combinablePromotions("P3", examplePromotions) mustBe Seq(
          PromotionCombo(Seq("P3", "P2")),
          PromotionCombo(Seq("P3", "P4", "P5"))
        )
