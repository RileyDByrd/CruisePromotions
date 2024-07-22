package byrd.riley.cruisepromotions

import InsertionOrderSet.`+:`

import scala.annotation.tailrec

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

def combinablePromotions(
  promotionCode: String,
  allPromotions: Seq[Promotion]
): Seq[PromotionCombo] = allCombinablePromotions(allPromotions)
  .filter(promotionCombo => promotionCombo.promotionCodes.contains(promotionCode))
  .map(promotionCombo => PromotionCombo(promotionCode +: promotionCombo.promotionCodes.filterNot(_ == promotionCode)))

def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
  // Convert allPromotions to an InsertionOrderSet since each Promotion must be unique.
  val allPromotionSet = allPromotions.to(InsertionOrderSet)
  // Find the largest combinable set of promotions for each promotion and add them to one set so that duplicates are
  // eliminated.
  allPromotionSet.foldLeft(InsertionOrderSet.empty[InsertionOrderSet[Promotion]]):
    case (combinablePromotions, target) =>
      combinablePromotions ++ findLargestPromotionCombinationsFor(target, allPromotionSet - target)
  // Represent each combinable set of promotions as a PromotionCombo instance and sort alphanumerically descending
  // inside and across PromotionCombos.
  .map(promotions => PromotionCombo(promotions.map(_.code).toSeq.sorted)).toSeq.sortBy(_.promotionCodes.mkString)

def findLargestPromotionCombinationsFor(
  target: Promotion, allOtherPromotions: InsertionOrderSet[Promotion]
): InsertionOrderSet[InsertionOrderSet[Promotion]] =
  // Remove promotions not combinable with target.
  val promotionsCombinableWithTarget = allOtherPromotions
    .filterNot(promotion => target.notCombinableWith.contains(promotion.code))
  // Find all combinations involving the target.
  val promotionCombinations = findPromotionCombinationsBranching(promotionsCombinableWithTarget)
  // Find the size of the largest combination.
  val sizeOfLargestCombination = promotionCombinations.map(_.size).max

  // Return only the largest combinations.
  promotionCombinations.filter(_.size == sizeOfLargestCombination)
    // Include the target in the combinations since the method began with promotions other than the target.
    .map(InsertionOrderSet(target) ++ _)


/**
 * Find all the promotion combinations in a provided Set.
 *
 * @param remainingPromotions   The provided Set and the promotions yet to be examined for combinations.  This is shifted
 *                              (i.e. consumed from the head) once each recursion and repopulated from branchesToReturnTo
 *                              if there are more promotions to examine.
 * @param promotionsFound       The promotions found to be combinable.  This is added to promotionsFromAllRuns and reset to
 *                              an empty Set after remainingPromotions has been exhausted.
 * @param promotionsFromAllRuns The promotions from previous runs through remainingPromotions found to be combinable
 *                              and the value that is ultimately returned.  promotionsFound is added to this Set every
 *                              time remainingPromotions is exhausted.
 * @param branchesToReturnTo    Each recursion, the head of remainingPromotions is used to filter non-combinable promotions
 *                              out of the rest of the remainingPromotions Set.  In case doing so eliminates matches that
 *                              would result in a larger Set, the tail is appended to branchesToReturnTo, which will
 *                              eventually repopulate remainingPromotions to go through the same process without the head.
 * @return All the promotion combinations in a provided Set.
 */
@tailrec
def findPromotionCombinationsBranching(
  remainingPromotions: InsertionOrderSet[Promotion],
  promotionsFound: InsertionOrderSet[Promotion] = InsertionOrderSet.empty,
  promotionsFromAllRuns: InsertionOrderSet[InsertionOrderSet[Promotion]] = InsertionOrderSet.empty,
  branchesToReturnTo: InsertionOrderSet[InsertionOrderSet[Promotion]] = InsertionOrderSet.empty
): InsertionOrderSet[InsertionOrderSet[Promotion]] =
  remainingPromotions match
    case head +: tail =>
//      println(s"head: $head, tail: $tail")
      val promotionsCombinableWithHead = (remainingPromotions - head)
        .filterNot(promotion => head.notCombinableWith.contains(promotion.code))
      findPromotionCombinationsBranching(
        remainingPromotions = promotionsCombinableWithHead,
        promotionsFound = promotionsFound + head,
        promotionsFromAllRuns = promotionsFromAllRuns,
        branchesToReturnTo = branchesToReturnTo + tail
      )
    case InsertionOrderSet() =>
      branchesToReturnTo match
        case trimmedBranch +: remainingBranches =>
//          println(s"trimmedBranch: $trimmedBranch, remainingBranches: $remainingBranches")
          findPromotionCombinationsBranching(
            remainingPromotions = trimmedBranch,
            promotionsFound = InsertionOrderSet.empty,
            promotionsFromAllRuns = promotionsFromAllRuns + promotionsFound,
            branchesToReturnTo = remainingBranches
          )
        case InsertionOrderSet() =>
          promotionsFromAllRuns + promotionsFound
