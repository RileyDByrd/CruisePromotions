package byrd.riley.cruisepromotions

import scala.annotation.targetName
import scala.collection.immutable.{ AbstractSet, StrictOptimizedSetOps }
import scala.collection.{ IterableFactory, IterableFactoryDefaults, mutable }

// For modeling promotions collectively, I wanted a structure that would provide the insertion order guarantee of a Seq
// so that I might use deconstructive pattern matching to recurse through it.  I also wanted the uniqueness guarantee of
// a Set so that duplicates, which I would certainly be adding to the collection, would be ignored.  Upon searching for
// this, I found that Scala offers a mutable version of this, called a LinkedHashSet, but not an immutable version.
// There is a feature request for this here, https://github.com/scala/scala-library-next/issues/22.  Since it has not
// been released, I sought to make my own.  I used the class here as an example, https://github.com/scalatest/scalatest/pull/2308/files,
// and augmented it with an IterableFactory to convert Seqs to InsertionOrderSets, SetOps to get the operations for
// adding and subtracting from Sets, a +: extractor to separate the head and tail for recursion, and unapplySeq to match
// exact Sets (like the empty Set).
object InsertionOrderSet extends IterableFactory[InsertionOrderSet]:
  def empty[A]: InsertionOrderSet[A] = new InsertionOrderSet(Seq.empty[A])

  def from[A](source: IterableOnce[A]): InsertionOrderSet[A] =
    source match
      case insertionOrderSet: InsertionOrderSet[A] => insertionOrderSet
      case _ if source.knownSize == 0 => empty[A]
      case _ => (newBuilder[A] ++= source).result()

  def newBuilder[A]: scala.collection.mutable.Builder[A, InsertionOrderSet[A]] = new mutable.GrowableBuilder[A, mutable.LinkedHashSet[A]](empty
    .underlying
  ).mapResult(new InsertionOrderSet(_))

  @targetName("InsertionOrderSetHeadToTailExtractor")
  object `+:`:
    def unapply[A](insertionOrderSet: InsertionOrderSet[A]): Option[(A, InsertionOrderSet[A])] = insertionOrderSet
      .headOption.map(_ -> insertionOrderSet.tail)

  def unapplySeq[A](insertionOrderSet: InsertionOrderSet[A]): Option[Seq[A]] = Some(insertionOrderSet.toSeq)

class InsertionOrderSet[A](
  linkedHashSet: mutable.LinkedHashSet[A]
) extends AbstractSet[A] with StrictOptimizedSetOps[A, InsertionOrderSet, InsertionOrderSet[A]] with IterableFactoryDefaults[A, InsertionOrderSet]:
  def this(elements: Seq[A]) = this(mutable.LinkedHashSet(elements *))

  private val underlying = linkedHashSet

  override def contains(elem: A): Boolean = underlying.contains(elem)

  override def iterator: Iterator[A] = underlying.iterator

  override def excl(elem: A): InsertionOrderSet[A] = new InsertionOrderSet(linkedHashSet.filter(_ != elem))

  override def incl(elem: A): InsertionOrderSet[A] =
    if (underlying.contains(elem))
      this
    else
      new InsertionOrderSet(underlying.toSeq :+ elem)

  override def iterableFactory: IterableFactory[InsertionOrderSet] = InsertionOrderSet
