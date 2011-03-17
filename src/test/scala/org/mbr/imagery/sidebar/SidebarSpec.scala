package org.mbr.imagery.sidebar

import org.specs.Specification

/**
 * User: mathias
 * Date: 17.03.11 21:36
 * Time: 21:36
 */

class SidebarSpec extends Specification {

  "Sidebar" should {

    "define plain root element without childs" in {
      val root = SomeSidebarElement("root", None, Nil)
      val navigation = Sidebar(root).navigation

      "element node must be root" in {
        navigation.element must beEqual(root)
      }

      "is selected" in {
        navigation.selected must beTrue
      }

      "is open" in {
        navigation.open must beTrue
      }

      "empty childs" in {
        navigation.children must beEmpty
      }
    }

    "root element with childs" in {
      val root = new SidebarElement {

        self =>

        type Element = SidebarElement

        def children = SomeSidebarElement("1", Some(self), Nil) :: SomeSidebarElement("2", Some(self), Nil) :: Nil

        def parent = None

      }

      val selectedElement = root.children.head

      val navigation = Sidebar(selectedElement).navigation

      "element node must be root" in {
        navigation.element must beEqual(root)
      }

      "root is not selected" in {
        navigation.selected must beFalse
      }

      "root is open" in {
        navigation.open must beTrue
      }

      "root has childs" in {
        navigation.children.size must be(2)

        val first = navigation.children.head

        "first child is selected" in {
          first.selected must beTrue
        }

        "second child is not selected" in {
          val second = navigation.children.toList(1)
          second.selected must beFalse
        }
      }
    }
  }
}

case class SomeSidebarElement(id: String,
                              parent: Option[SidebarElement],
                              children: Iterable[SidebarElement]) extends SidebarElement{
  type Element = SidebarElement
}