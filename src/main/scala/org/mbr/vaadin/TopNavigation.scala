package org.mbr.vaadin

import Vaadin._
import org.mbr.imagery.image.Image
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
    Activated
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

  def register(action: Action) = {
    new {
      def click(exec: => Unit): BoundAction = {
        click(_ => exec)
      }

      def click(exec: ExecutionContext => Unit): BoundAction = {
        val button = new Button(action.label)
        button click {
          val context = new ExecutionContext {
            def notify(message: Option[String], progress: Option[Double]) = {}
          }
          exec(context)
        }
        for (description <- action.tooltip) {
          button.setDescription(description)
        }
        button.setEnabled(action.enabled)
        // TODO: add icon
        container.addComponent(button)
        new BoundButtonAction(action, exec, button)
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

trait Action {

  def label: String

  def icon: Option[Blob]

  def tooltip: Option[String]

  /**
   * @return true if the action is enabled otherwise false
   */
  def enabled: Boolean
}

object Action {

  def apply(label: String,
            icon: Option[Blob] = None,
            tooltip: Option[String] = None,
            enabled: Boolean = true): Action = {
    DefaultAction(label, icon, tooltip, enabled)
  }
}

case class DefaultAction(label: String, icon: Option[Blob] = None, tooltip: Option[String] = None, enabled: Boolean = true) extends Action

trait Executable {

  /**
   * execute this unit of work by using the supplied context
   */
  def apply(context: ExecutionContext): Unit
}

/**
 * define some context to run the unit
 */
trait ExecutionContext {

  /**
   * Called during execution to notify the caller about the current state of the execution
   *
   * @param message Some verbose message to identify the state or None if no message could be supplied
   * @param progress: Some progress value between 0 and 100 % or None if no progress can be supplied
   */
  def notify(message: Option[String] = None, progress: Option[Double] = None): Unit
}

class BoundButtonAction(action: Action,
                        exec: ExecutionContext => Unit,
                        button: Button) extends BoundAction {

  def enable = {
    button.setEnabled(true)
  }

  def disable = {
    button.setEnabled(false)
  }

  def enabled = {
    button.isEnabled
  }

  def tooltip = {
    action.tooltip
  }

  def icon = {
    action.icon
  }

  def label = {
    action.label
  }

  def apply(context: ExecutionContext): Unit = {
    exec(context)
  }
}

trait BoundAction extends Action with Executable {

  /**
   * disable this action
   */
  def disable: Unit

  /**
   * enable this action
   */
  def enable: Unit
}