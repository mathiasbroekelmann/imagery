package org.mbr.vaadin

import Vaadin._
import com.vaadin.ui.{ComponentContainer, Button, CssLayout}
import Extension._

/**
 * top navigation extension
 *
 * User: mathias
 * Date: 27.03.11 21:13
 * Time: 21:13
 */
object TopNavigation extends Extension {

  def start(context: ExtensionContext) = {
    context.activateWith[ApplicationFrame] { enable(_, context) }
    context.activate {
      case frame: ApplicationFrame => enable(frame, context)
    }
  }

  def enable(frame: ApplicationFrame, context: ExtensionContext): Activation = {
    val registration = context.register(new ButtonTopNavigation {
      val container = {
        val navigation = new CssLayout
        navigation.addStyleName("navigation")
        frame.top.addComponent(navigation)
        navigation
      }

      def dispose = frame.top.removeComponent(container)
    })
    dispose(registration.dispose)
  }
}

trait ButtonTopNavigation extends TopNavigation {

  def container: ComponentContainer

  def context: ExecutionContext = {
    new ExecutionContext {
      def notify(message: Option[String], progress: Option[Double]) = {}
    }
  }

  def register(action: Action) = {
    new {
      def click(exec: => Unit): BoundAction = {
        click(_ => exec)
      }

      def click(exec: ExecutionContext => Unit): BoundAction = {

        val button = new Button(action.label)

        for (description <- action.tooltip) {
          button.setDescription(description)
        }
        button.setEnabled(action.enabled)
        // TODO: add icon

        container.addComponent(button)

        bind(action calling exec, button)
      }

      def bind(action: ExecutableAction, button: Button, listener: Option[Button.ClickListener] = None): BoundButtonAction = {
        listener match {
          case Some(l) => {
            button.removeListener(l)
          }
          case _ =>
        }
        val newListener = button click action(context)
        new BoundButtonAction(action, button) {
          def calling(f: (ExecutionContext) => Unit) = bind(action calling f, button, Some(newListener))

          def unbind = {
            container.removeComponent(button)
          }
        }
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
  def register(action: Action): {
    def click(f: => Unit): BoundAction
    def click(f: ExecutionContext => Unit): BoundAction
  }
}