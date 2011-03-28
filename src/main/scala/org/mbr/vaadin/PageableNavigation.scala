package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}

/**
 * define the activated extensions for the application
 */
trait PageableNavigation {

  type Page

  /**
   * navigate to some page
   */
  def goto: {
    def first: Page
    def last: Page
    def next: Option[Page]
    def previous: Option[Page]
    def apply(pageIndex: Int): Option[Page]
  }
}

/**
 * User: mathias
 * Date: 27.03.11 16:14
 * Time: 16:14
 */


/**
 * a navigation mixin trait to navigate through pages
 */
