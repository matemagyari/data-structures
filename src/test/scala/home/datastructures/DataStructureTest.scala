package home.datastructures

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.{IndexedSeqView, SeqView, immutable}
import scala.collection.immutable.{HashSet, TreeSet}
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

class DataStructureTest extends AnyFlatSpec with Matchers {

  val times = 100

  val small = 10
  val middle = 100
  val big = 10000
  val biggest = 100000

  //no intermediate results if views transformed multiple times
  "Views" should "be lazy" in {

    val v: IndexedSeqView[Int] = (1 to 10).view

    val counter = new AtomicInteger()

    //map is lazy on the view
    val v2: IndexedSeqView[Int] = v.map { i ⇒
      counter.incrementAndGet()
      i + 10
    }
    counter.get shouldBe 0

    val v3: Seq[Int] = v2.toSeq
    counter.get shouldBe 10
    v3 shouldBe (11 to 20)
  }

  "Iterators" should "be lazy" in {

    def doNothing(a: Any) = {}

    val it: Iterator[Int] = (1 to middle).toIterator

    val counter = new AtomicInteger()
    val it2 = it.map { i ⇒
      counter.incrementAndGet()
    }

    counter.get() shouldBe 0

    //take does still return lazy seq
    it2.take(10)
    counter.get() shouldBe 0

    it2.take(10).foreach(doNothing)
    counter.get() shouldBe 10
  }

  "List" should "take constant time for add/remove head" in {

    val list1 = (1 to middle).toList
    val list2 = (1 to biggest).toList

    def time1 = avgTime {
      0 +: list1
    }

    def time2 = avgTime {
      0 +: list2
    }

    println(time1)
    println(time2)
  }

  "List" should "take linear time for add/remove element at the end" in {

    val list1 = (1 to small).toList
    val list2 = (1 to biggest).toList

    def time1 = avgTime {
      list1 :+ 0
    }

    def time2 = avgTime {
      list2 :+ 0
    }

    println(time1)
    println(time2)
  }

  "HashMap" should "take constant time for lookup/add/remove" in {}

  "HashMap" should "take linear time for finding minimum key" in {}

  "HashSet" should "take constant time for lookup/add/remove" in {

    def createSet(size: Int): HashSet[Int] =
      Random.shuffle((1 to size)).foldLeft(HashSet.empty[Int]) { (acc, key) ⇒
        acc + key
      }

    val s1 = createSet(small)
    val s2 = createSet(100000)

    verifySameOrderOfMagnitude(
      computation1 = { s1.contains(1) },
      computation2 = { s2.contains(1) }
    )

  }

  "HashSet" should "take linear time for finding minimum element" in {}

  "TreeMap" should "take log time for lookup/add/remove/min key" in {}

  "TreeSet" should "take log time for lookup/add/remove/min element" in {

    def createSet(size: Int): TreeSet[Int] =
      Random.shuffle((1 to size)).foldLeft(TreeSet.empty[Int]) { (acc, key) ⇒
        acc + key
      }

    val s1 = createSet(middle)
    val s2 = createSet(biggest)

    def comp1 = { s1.contains(s1.size / 2) }
    def comp2 = { s2.contains(s2.size / 2) }

    avgTime(comp1)
    avgTime(comp2)

    val time1 = avgTime(comp1)
    val time2 = avgTime(comp2)

    println(time1)
    println(time2)

  }

  private def verifySameOrderOfMagnitude(computation1: ⇒ Unit, computation2: ⇒ Unit): Unit = {
    def order(v: Long): BigDecimal = {
      val result = BigDecimal(Math.log10(v)).setScale(0, RoundingMode.HALF_UP)
      println(s"$v $result")
      result
    }
    //warm up
    avgTime(computation1)
    avgTime(computation2)

    val time1 = avgTime { computation1 }
    val time2 = avgTime { computation2 }
    order(time1) shouldBe order(time2)
  }

  private def avgTime(computation: ⇒ Unit): Long =
    (1 to times).map(_ ⇒ time(computation)).drop(times / 2).sum

  private def time(computation: ⇒ Unit): Long = {
    val start = System.nanoTime()
    computation
    System.nanoTime() - start
  }

}
