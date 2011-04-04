package org.mbr.vaadin

import org.mbr.imagery.blob.Blob
import com.vaadin.ui.Button
import Vaadin._

/**
 * User: mathias
 * Date: 28.03.11 20:35
 * Time: 20:35
 */
trait Action {

  type CallingAction <: ExecutableAction

  /**
   * defines the label of the action
   */
  def label: String

  /**
   * define some optional icon
   */
  def icon: Option[Blob]

  /**
   * define some optional tooltip / description
   */
  def tooltip: Option[String]

  /**
   * @return true if the action is enabled otherwise false
   */
  def enabled: Boolean

  /**
   * creates an executable action by using this action with the provied function to execute.
   */
  def calling(f: ExecutionContext => Unit): CallingAction

  def calling(f: => Unit): CallingAction = calling(_ => f)
}

object Action {

  def apply(label: String,
            icon: Option[Blob] = None,
            tooltip: Option[String] = None,
            enabled: Boolean = true): Action = {
    new DefaultAction(label, icon, tooltip, enabled)
  }
}

class DefaultAction(val label: String,
                    val icon: Option[Blob] = None,
                    val tooltip: Option[String] = None,
                    val enabled: Boolean = true) extends Action {

  type CallingAction = ExecutableAction

  def calling(f: (ExecutionContext) => Unit) = new DefaultAction(label, icon, tooltip, enabled) with ExecutableAction {
    def apply(context: ExecutionContext) = f(context)
  }
}

/**
 * defines an action which is executable
 */
trait ExecutableAction extends Action with Executable

/**
 * defines an executable which can be executed at some time.
 */
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

abstract class BoundButtonAction(val action: ExecutableAction,
                                 val button: Button) extends BoundAction {

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

  def apply(context: ExecutionContext): Unit = action(context)
}

/**
 * some action which has been bound to a component
 */
trait BoundAction extends ExecutableAction with Disposable {

  type CallingAction = BoundAction

  /**
   * disable this action
   */
  def disable: Unit

  /**
   * enable this action
   */
  def enable: Unit

  def unbind: Unit

  def dispose = unbind
}