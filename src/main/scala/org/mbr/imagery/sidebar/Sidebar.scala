package org.mbr.imagery.sidebar

/**
 * User: mathias
 * Date: 17.03.11 20:11
 * Time: 20:11
 */
trait Sidebar {

  /**
   * provides the root navigation element for the sidebar
   */
  def navigation: Navigation
}

trait SidebarElement {

  type Element <: SidebarElement

  /**
   * @return Some parent element or none if this is a root element
   */
  def parent: Option[Element]

  /**
   * @return
   */
  def children: Iterable[Element]
}

/**
 * a navigation node builds up an hierarchical navigation structure
 */
trait Navigation {
  def element: SidebarElement

  def selected: Boolean

  def open: Boolean

  def children: Iterable[Navigation]
}

object Sidebar {

  def apply(selectedElement: SidebarElement): Sidebar = {
    new Sidebar {
      lazy val navigation = {
        // determine path to root
        def parentPath(element: SidebarElement, parent: Option[SidebarElement]): List[SidebarElement] = {
          (parent match {
            case None => Nil
            case Some(p) => parentPath(p, p.parent)
          }) :+ element
        }

        val path = parentPath(selectedElement, selectedElement.parent)

        def childsOf(element: SidebarElement): Iterable[Navigation] = {
          for (child <- element.children.toStream) yield {
            new Navigation {
              val element = child
              lazy val selected = child == selectedElement
              lazy val open = path.find(_ == child).isDefined
              lazy val children = childsOf(child)
            }
          }
        }

        val root = path.head
        new Navigation {
          val element = root
          lazy val selected = root == selectedElement
          lazy val open = true
          lazy val children = childsOf(root)
        }
      }
    }
  }
}