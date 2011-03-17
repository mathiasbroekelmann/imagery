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
case class Navigation(element: SidebarElement,
                      selected: Boolean,
                      open: Boolean,
                      children: Iterable[Navigation])

object Sidebar {

  def apply(selectedElement: SidebarElement): Sidebar = {
    new Sidebar {
      lazy val navigation = {
        // determine path to root
        def parentPath(element: SidebarElement): List[SidebarElement] = {
          element.parent match {
            case None => List(element)
            case Some(parent) => parentPath(parent) :+ element
          }
        }

        val path = parentPath(selectedElement)

        def childsOf(element: SidebarElement): Iterable[Navigation] = {
          for(child <- element.children.toStream) yield {
            Navigation(element = child,
              selected = child == selectedElement,
              open = path.find(_ == element).isDefined,
              children = childsOf(child))
          }
        }

        val root = path.head
        Navigation(element = root,
          selected = root == selectedElement,
          open = true,
          children = childsOf(root))
      }
    }
  }
}