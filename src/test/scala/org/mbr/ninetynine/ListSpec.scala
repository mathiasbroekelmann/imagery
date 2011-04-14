package org.mbr.ninetynine

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.math._

/**
 * @see http://aperiodic.net/phil/scala/s-99/
 *
 * @author mathias.broekelmann
 * @since 11.04.11 10:38
 */
class ListSpec extends Spec with ShouldMatchers {

  describe("Lists") {

    import Lists._

    val list = List(1, 1, 2, 3, 5, 8)

    it("P01 find last element of a list") {
      last(list) should equal(8)
    }

    it("P02 Find the last but one element of a list") {
      penultimate(list) should equal(5)
    }

    it("P03 Find the Kth element of a list") {
      nth(2, list) should equal(2)
    }

    it("P04 Find the number of elements of a list") {
      Lists.length(list) should equal(6)
    }

    it("P05 Reverse a list") {
      reverse(list) should equal(list.reverse)
    }

    it("P06 Find out whether a list is a palindrome") {
      isPalindrome(List(1, 2, 3, 2, 1)) should equal(true)
      isPalindrome(List(1, 2, 2, 1)) should equal(true)
      isPalindrome(List(1, 2, 3, 4, 5)) should equal(false)
    }

    it("P07 Flatten a nested list structure") {
      flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should equal(List(1, 1, 2, 3, 5, 8))
    }

    it("P08 Eliminate consecutive duplicates of list elements") {
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should equal(List('a, 'b, 'c, 'a, 'd, 'e))
    }

    it("P09 Pack consecutive duplicates of list elements into sublists") {
      val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
      pack(given) should equal(expected)
    }

    it("P10 Run-length encoding of a list") {
      val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      encode(given) should equal(expected)
    }

    it("P11 Modified run-length encoding") {
      val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
      encodeModified(given) should equal(expected)
    }

    it("P12 Decode a run-length encoded list") {
      val given = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      val expected = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      decode(given) should equal(expected)
    }

    it("P13 Run-length encoding of a list (direct solution)") {
      val given = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      encodeDirect(given) should equal(expected)
    }

    it("P14 Duplicate the elements of a list") {
      duplicate(List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }

    it("P15 Duplicate the elements of a list a given number of times") {
      duplicateBy(3, List('a, 'b, 'c, 'c, 'd)) should equal(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    }

    it("P16 Drop every Nth element from a list") {
      dropNthElement(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    }

    it("P17 Split a list into two parts") {
      val given = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expected = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      splitAt(3, given) should equal(expected)
    }

    it("P18 Extract a slice from a list") {
      val given = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expected = List('d, 'e, 'f, 'g)
      slice(3, 7, given) should equal(expected)
    }

    it("P19 Rotate a list N places to the left") {
      val given = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      rotate(3, given) should equal(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
      rotate(-2, given) should equal(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    }

    it("P20 Remove the Kth element from a list") {
      val given = List('a, 'b, 'c, 'd)
      removeAt(1, given) should equal((List('a, 'c, 'd), 'b))
    }

    it("P21 Insert an element at a given position into a list") {
      val given = List('a, 'b, 'c, 'd)
      insertAt(1, 'new, given) should equal(List('a, 'new, 'b, 'c, 'd))
    }

    it("P22 Create a list containing all integers within a given range") {
      range(4, 9) should equal(List(4, 5, 6, 7, 8, 9))
    }

    it("P23 Extract a given number of randomly selected elements from a list") {
      val given = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
      val selected = randomSelect(3, given)
      selected.distinct.length should equal(3)
    }
  }
}

object Lists {

  @tailrec
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case head :: Nil => head
      case head :: tail => last(tail)
    }
  }

  @tailrec
  def penultimate[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case head :: last :: Nil => head
      case head :: tail => penultimate(tail)
    }
  }

  def nth[A](position: Int, list: List[A]): A = {

    def invalidPosition = {
      throw new NoSuchElementException("Invalid position: " + position +
        " to apply to list with a size of: " + list.size)
    }

    @tailrec
    def nthRecursive(index: Int, list: List[A]): A = {
      (index, list) match {
        case (x, _) if x < 0 => invalidPosition
        case (_, Nil) => invalidPosition
        case (0, head :: _) => head
        case (x, head :: tail) => nthRecursive(x - 1, tail)
      }
    }
    nthRecursive(position, list)
  }

  def length(list: List[_]): Int = {
    @tailrec
    def length(existingCount: Int, tail: List[_]): Int = {
      tail match {
        case Nil => existingCount
        case head :: tail => length(existingCount + 1, tail)
      }
    }
    length(0, list)
  }

  def reverse[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case head :: tail => reverse(tail) :+ head
    }
  }

  def isPalindrome(list: List[_]): Boolean = {
    val halfOfListSize = length(list) / 2
    list.take(halfOfListSize).reverse == list.takeRight(halfOfListSize)
  }

  def flatten(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case (head: List[_]) :: tail => flatten(head) ++ flatten(tail)
      case head :: tail => head +: flatten(tail)
    }
  }

  def compress[A](list: List[A]): List[A] = {

    @tailrec
    def compress(compressedList: List[A], remainingList: List[A]): List[A] = {
      remainingList match {
        case Nil => compressedList
        case head :: tail => compress(compressedList :+ head, tail.dropWhile(_ == head))
      }
    }
    compress(Nil, list)
  }

  def pack[A](list: List[A]): List[List[A]] = {

    @tailrec
    def pack(packedList: List[List[A]], remainingList: List[A]): List[List[A]] = {
      remainingList match {
        case Nil => packedList
        case head :: tail => {
          val (headGroup, tail) = remainingList.span(_ == head)
          pack(packedList :+ headGroup, tail)
        }
      }
    }

    pack(Nil, list)
  }

  def encode[A](list: List[A]): List[(Int, A)] = {
    for (group <- pack(list)) yield {
      (group.length, group.head)
    }
  }

  def encodeModified[A](list: List[A]): List[Any] = {
    for (group <- encode(list)) yield {
      if (group._1 == 1) {
        group._2
      } else {
        group
      }
    }
  }

  def encodeModifiedTypeSafe[A](list: List[A]): List[Either[A, (Int, A)]] = {
    for (group <- encode(list)) yield {
      if (group._1 == 1) {
        Left(group._2)
      } else {
        Right(group)
      }
    }
  }

  def decode[A](encodedList: List[(Int, A)]): List[A] = {
    encodedList.flatMap {encoded => List.fill(encoded._1)(encoded._2)}
  }

  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    list match {
      case Nil => Nil
      case head :: tail => {
        val (packed, next) = list.span(_ == head)
        (packed.length, head) +: encodeDirect(next)
      }
    }
  }

  def duplicate[A](list: List[A]): List[A] = {
    list.flatMap(element => List(element, element))
  }

  def duplicateBy[A](count: Int, list: List[A]): List[A] = {
    list.flatMap {List.fill(count)(_)}
  }

  def dropNthElement[A](number: Int, list: List[A]): List[A] = {
    list.zipWithIndex.filter(indexedElement => (indexedElement._2 + 1) % number != 0).map(_._1)
  }

  def splitAt[A](index: Int, list: List[A]): (List[A], List[A]) = {
    (list.take(index), list.drop(index))
  }

  @tailrec
  def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
    (max(0, start), max(0, end), list) match {
      case (_, _, Nil) => Nil
      case (0, e, list) => list.take(e)
      case (s, e, head :: tail) => slice(s - 1, e - 1, tail)
    }
  }

  def rotate[A](numberOfPlaces: Int, list: List[A]): List[A] = {
    val elementsToDrop = if (numberOfPlaces > 0) {
      numberOfPlaces
    } else {
      list.length + numberOfPlaces
    }
    (list ++ list).drop(elementsToDrop).take(list.length)
  }

  def removeAt[A](index: Int, list: List[A]): (List[A], A) = {

    @tailrec
    def removeAt(index: Int, left: List[A], right: List[A]): (List[A], A) = {
      (index, right) match {
        case (_, Nil) => throw new NoSuchElementException
        case (x, _) if x < 0 => throw new NoSuchElementException
        case (x, head :: tail) if x == 0 => (left ++ tail, head)
        case (x, head :: tail) => removeAt(x - 1, left :+ head, tail)
      }
    }
    removeAt(index, Nil, list)
  }

  def insertAt[A, B >: A](index: Int, value: B, list: List[A]): List[B] = {
    list.splitAt(index) match {
      case (pre, post) => pre ::: value :: post
    }
  }

  def range(first: Int, last: Int): List[Int] = {
    (first to last).toList
  }

  def randomSelect[A](amount: Int, list: List[A]): List[A] = (amount, list) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (amount, list) => {
      val selectedIndex = new util.Random().nextInt(list.length)
      val (remaining, selected) = removeAt(selectedIndex, list)
      selected :: randomSelect(amount - 1, remaining)
    }
  }
}