package home.datastructures

import scala.collection.immutable.Seq
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Example0 {

  /**
    * 12.1 If you were integrating a feed of end of day stock price information (open, high, low,
    * and closing price) for 5,000 companies, how would you do it? You are responsible for
    * the development, rollout and ongoing monitoring and maintenance of the feed. Describe
    * the different methods you considered and why you would recommend your
    * approach. The feed is delivered once per trading day in a comma-separated format
    * via an FTP site. The feed will be used by 1000 daily users in a web application
    */

  /**
  * - Q1: how much latency can we afford
  * - if we represent every price in a double (8 bytes), then 8 * 4 * 5000 = 160 000 bytes = 160 KB
  * - clients could connect by HTTP, WS, RSS(?)
  * - poll the FTP site (maybe in a time window), save in a DB
  * - today's data can be kept in memory. Actually thousands of days can be
  * - 1000 users = single machine is enough. More users => use a load balancer
  */
}

object Example1 {

  /**
    * How would you design the data structures for a very large social network (Facebook,
    * LinkedIn, etc)? Describe how you would design an algorithm to show the connection,
    * or path, between two people (e.g., Me -> Bob -> Susan -> Jason -> You).
    */
  /**
    * - store users in one table - only id
    * - store different kinds of data in their own tables (personal_details, interests, ...), referring to the user table
    *  - store connections separately - graph DB. Or store links in relational db (id -> id)
    *  - shortest path graph algorithm. or a naively breadth-first search
    */
  final case class Link(x: Int, y: Int)

  val links: Set[Link] = Set(Link(1, 2), Link(2, 3), Link(2, 4), Link(3, 5))

//  def findPath(links: Set[Link], from: Int, to: Int): Seq[Int] = {
//    def helper(links: Set[Link], from: Int, to: Int, path: Seq[Int]): Seq[Int] = {
//      if (links.exists(link ⇒ link.x == to || link.y == to))
//        path
//      else {
//
//      }
//    }
//  }

}

object Example11 {

  /**
    * Given an input file with four billion integers, provide an algorithm to generate an
    * integer which is not contained in the file. Assume you have 1 GB of memory.
    * FOLLOW UP
    * What if you have only 10 MB of memory?
    */

  /**
  * Answer:
  *
  * One integer is 2^32 bit = 4 byte
  * 4 billion (around 2^30 * 4 = 2^32) integers is 16 billion bytes = 16 GB
  *  => reading up the whole file is not good enough
  *
  *  4 billion is around 2^32. That is an integer. We can assign index an element for each integer in the bit sequence
  *  Once we have the
  */

}

object Example2 extends App {

  /**
    * You have an array with all the numbers from 1 to N, where N is at most 32,000. The
    * array may have duplicate entries and you do not know what N is. With only 4KB of
    * memory available, how would you print all duplicate elements in the array?
    */
  /**
    *  Answer:
    *
    *  One integer is 2^32 bit = 4 byte. I can hold at most a thousand integers in memory
    *
    *  Original array is O. Create a new array list (N). Go through O one and one by one add elements to N. Before adding element X
    *  go through N and if it already has X, print it. X should be removed from O
    */
  val o: ArrayBuffer[Int] = scala.collection.mutable.ArrayBuffer[Int](1, 2, 3)
  val n = ListBuffer.empty[Int]

  while (o.nonEmpty) {
    val elem = o.remove(o.size - 1)
    if (n.contains(elem)) {
      println(s"Duplicate: $elem")
    }
    else {
      n.append(elem)
    }
  }

}

object Example3 {

  /**
  *
  * 12.6 You have a billion urls, where each is a huge page. How do you detect the duplicate
  * documents?
  */
}
