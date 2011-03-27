package org.mbr.vaadin

import scala.math._

/**
 * allows paging over elements
 *
 * @param pageIndex the zero based index of this pageable
 * @param allElements all elements to page over
 * @param pageSize the number of elements for each page
 *
 * User: mathias
 * Date: 27.03.11 16:14
 * Time: 16:14
 */
case class Pageable[A](allElements: Iterable[A],
                       pageIndex: Int = 0,
                       pageSize: Option[Int] = Some(10)) {

  self =>

  protected def build(pageIndex: Int): Pageable[A] = {
    if (pageIndex == self.pageIndex) {
      self
    } else {
      Pageable(allElements, pageIndex, pageSize)
    }
  }

  /**
   * get a specific page
   */
  def get = new {

    /**
     * get the first page
     */
    def first: Pageable[A] = build(0)

    /**
     * get the last page.
     * Getting the last page run indefinitly if the provided iterable is infinite.
     */
    def last: Pageable[A] = pageSize match {
      case Some(size) => build(max(0, (allElements.size - 1) / size))
      case None => self
    }

    /**
     * Some previous page or None if this is the first page
     */
    def previous: Option[Pageable[A]] = {
      apply(pageIndex - 1)
    }


    /**
     * Some next page or None if this is the last page
     */
    def next: Option[Pageable[A]] = {
      apply(pageIndex + 1)
    }

    /**
     * Some page for the given pageindex or None if the pageindex is not valid
     */
    def apply(pageIndex: Int): Option[Pageable[A]] = pageSize match {
      case _ if pageIndex == 0 => Some(first)
      case _ if pageIndex == self.pageIndex => Some(self)
      case Some(size) if pageIndex < 0 => None
      case Some(size) if allElements.drop(pageIndex * size).isEmpty => None
      case Some(size) => Some(build(pageIndex))
      case _ => None
    }
  }

  /**
   * @return all pageables starting with the first one.
   */
  def pages: Iterable[Pageable[A]] = {
    pageSize match {
      case None => self :: Nil
      case Some(size) => {
        val first = get first
        def next(page: Pageable[A]): Stream[Pageable[A]] = page.get.next match {
          case None => Stream.empty
          case Some(pageable) => Stream.cons(pageable, next(pageable))
        }
        Stream.cons(first, next(first))
      }
    }
  }

  /**
   * @return true if this pageable is the first page otherwise false
   */
  def first: Boolean = pageIndex == 0

  /**
   * @return true if this pageable is the last page otherwise false
   */
  def last: Boolean = pageSize match {
    case Some(size) => allElements.drop((pageIndex + 1) * size).isEmpty
    case None => true
  }

  /**
   * the elements of the pageable
   */
  def elements: Iterable[A] = pageSize match {
    case Some(size) => allElements.slice(pageIndex * size, size)
    case None => allElements
  }
}