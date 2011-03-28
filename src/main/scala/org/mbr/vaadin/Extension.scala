package org.mbr.vaadin

import com.vaadin.Application
import com.vaadin.ui.ComponentContainer

/**
 * User: mathias
 * Date: 27.03.11 20:08
 * Time: 20:08
 */

/**
 * defines an extension in the application. Extensions are normally collected in some way for the application.
 * The order of the extension doesn't matter.
 */
trait Extension {

  /**
   * initialize the extension
   */
  def start(context: ExtensionContext): Option[ActivatedExtension]
}

/**
 * defines an activated extension which can be stopped.
 */
trait ActivatedExtension {

  def stop: Unit
}

/**
 * default result for activated extensions which don't have any jobs to do on #stop
 */
object Activated extends ActivatedExtension with Activation  {

  def stop = {}

  def deactivate = {}
}

trait ExtensionContext {

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
   * @return the extension point which is registered
   */
  def extensionPoint: ExtensionPoint

  /**
   * unregisters the extension point.
   */
  def unregister: Unit = {}
}

/**
 * defines an extension point
 */
trait ExtensionPoint {

  def unregistered: Unit = {}
}

class DefaultExtensionContext extends ExtensionContext {

  case class ActiveActivation(extension: ExtensionPoint,
                              pf: PartialFunction[ExtensionPoint, Option[Activation]],
                              activation: Activation) extends Activation {

    def deactivate = {
      activation.deactivate
    }
  }

  var registeredActivations: List[PartialFunction[ExtensionPoint, Option[Activation]]] = Nil

  var extensionPoints: List[ExtensionPoint] = Nil

  var activations: List[ActiveActivation] = Nil

  def activationCandidate(extension: ExtensionPoint, pf: PartialFunction[ExtensionPoint, Option[Activation]]): Option[Activation] = {
    if (pf.isDefinedAt(extension)) {
      for (activation <- pf(extension)) yield {
        activations = activations :+ ActiveActivation(extension, pf, activation)
        activation
      }
    } else {
      None
    }
  }

  def register(extension: ExtensionPoint) = {
    // go through all existing activations
    for (activate <- registeredActivations) {
      activationCandidate(extension, activate)
    }

    // keep the extension point
    extensionPoints = extensionPoints :+ extension
    new RegisteredExtensionPoint {
      override def unregister = {
        extensionPoints = extensionPoints.filterNot(_ == extension)
        for (activation <- activations; if activation.extension == extension) {
          activation.deactivate
        }
        activations = activations.filterNot(_.extension == extension)
      }

      def extensionPoint = {
        extensionPoint
      }
    }
  }

  def activate(pf: PartialFunction[ExtensionPoint, Option[Activation]]): RegisteredActivation = {
    for (extension <- extensionPoints) {
      activationCandidate(extension, pf)
    }
    registeredActivations = registeredActivations :+ pf

    new RegisteredActivation {
      def unregister = {
        for (activation <- activations; if activation.pf == pf) {
          activation.deactivate
        }
        activations = activations.filterNot(_.pf == pf)
      }
    }
  }
}
