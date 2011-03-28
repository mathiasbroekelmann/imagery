package org.mbr.vaadin

import com.vaadin.ui.{Button, CssLayout}

import Vaadin._

/**
 * top navigation extension
 *
 * User: mathias
 * Date: 27.03.11 21:13
 * Time: 21:13
 */
object TopNavigation extends Extension {

  def start(context: ExtensionContext) = {
    context.activate {
      case frame: ApplicationFrame => Some(activate(frame, context))
    }
    Activated
  }

  def activate(frame: ApplicationFrame, context: ExtensionContext): Activation = {
    val top = frame.top
    val navigation = new CssLayout
    navigation.addStyleName("navigation")
    top.addComponent(navigation)

    println("registering top navigation")
    context.register(new TopNavigation {
      def action(label: String) = {
        new {
          def click(action: => Unit) = {
            val button = new Button(label)
            navigation.addComponent(button)
            button click action
            new Activation {
              def deactivate = {
                navigation.removeComponent(button)
              }
            }
          }
        }
      }

      override def unregistered = {
        top.removeComponent(navigation)
      }
    })

    new Activation {
      def deactivate = {
        top.removeComponent(navigation)
      }
    }
  }
}

/**
 * defines the extension point for the top navigation
 */
trait TopNavigation extends ExtensionPoint {

  /**
   * register an action for the top navigation
   */
  def action(label: String): {
    def click(action: => Unit): Activation
  }
}