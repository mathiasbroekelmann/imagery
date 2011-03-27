package org.mbr.vaadin

import com.vaadin.Application
import com.vaadin.ui.ComponentContainer

/**
 * User: mathias
 * Date: 27.03.11 20:08
 * Time: 20:08
 */

/**
 * defines an extension in the application
 */
trait Extension {

  /**
   * initialize the extension
   */
  def init(context: ExtensionContext): Unit
}

trait ExtensionContext {

  /**
   * @return the application frame
   */
  def application: ApplicationFrame

  /**
   * define a function which is applied when a matching extension point is registered in the application
   */
  def activate(pf: PartialFunction[ExtensionPoint, Option[Activation]]): RegisteredActivation

  /**
   * registers a new extension point.
   */
  def register(extensionPoint: ExtensionPoint): RegisteredExtensionPoint
}

/**
 * optional result of an applied function in ExtensionContext#activate which #deactivate function is called when the extension point is unregistered.
 */
trait Activation {
  /**
   * clean up any resources created by this activation if possible
   */
  def deactivate: Unit
}

/**
 * Result of a registered activation through ExtensionContext#activate. Allows unregistering the activation registration
 */
trait RegisteredActivation {

  /**
   * unregister the activation registration
   */
  def unregister: Unit
}

/**
 * defines a registered extension point
 */
trait RegisteredExtensionPoint {

  /**
   * @return the application where the extension is registered at
   */
  def application: ApplicationFrame

  /**
   * @return the extension point which is registered
   */
  def extensionPoint: ExtensionPoint

  /**
   * unregisters the extension point.
   */
  def unregister: Unit
}

/**
 * defines an extension point
 */
trait ExtensionPoint {
  def unregistered: Unit
}