package org.mbr.vaadin

import Vaadin._
import org.mbr.imagery.blob.Blob
import com.vaadin.ui.{ComponentContainer, Button, CssLayout}

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
    None
  }

  def activate(frame: ApplicationFrame, context: ExtensionContext): Activation = {
    val top = frame.top
    val navigation = new CssLayout
    navigation.addStyleName("navigation")
    top.addComponent(navigation)

    println("registering top navigation")
    context.register(new ButtonTopNavigation(navigation))
    new Activation {
      def deactivate = {
        top.removeComponent(navigation)
      }
    }
  }
}

class ButtonTopNavigation(container: ComponentContainer) extends TopNavigation {

  def context: ExecutionContext = new ExecutionContext {
    def notify(message: Option[String], progress: Option[Double]) = {}
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
          case Some(l) => button.removeListener(l)
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