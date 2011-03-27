package org.mbr.vaadin

import com.vaadin.ui.{Button, CssLayout}

import Vaadin._

/**
 * User: mathias
 * Date: 27.03.11 21:13
 * Time: 21:13
 */

/**
 * Some top navigation
 */
object TopNavigation extends Extension {

  def init(context: ExtensionContext) = {
    val top = context.application.top
    val navigation = new CssLayout
    navigation.addStyleName("navigation")
    top.addComponent(navigation)

    context.register(new TopNavigation {
      def action(label: String, f: => Unit) = {
        val button = new Button(label)
        navigation.addComponent(button)
        button.click(f)
      }

      def unregistered = {
        top.removeComponent(navigation)
      }
    })
  }
}

trait TopNavigation extends ExtensionPoint {

  /**
   * register an action for the top navigation
   */
  def action(label: String, f: => Unit)
}