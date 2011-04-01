package org.mbr.vaadin

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
  def start(context: ExtensionContext): Disposable
}

/**
 * defines optional functions which ease the implementation of extensions.
 */
object Extension {

  /**
   * wrap a function to be applied during cleanup.
   */
  def dispose(f: => Unit) = {
    new Disposable with ActivatedExtension with Activation with Registration {
      def dispose = f

      def deactivate = dispose

      def stop = dispose

      def unregister = dispose
    }
  }
}

import Extension._

/**
 * defines an activated extension which can be stopped.
 */
trait ActivatedExtension extends Disposable {
}

trait ExtensionContext {

  type Extension <: AnyRef

  /**
   * define a function which is applied when a matching extension point is registered in the application
   */
  def activate(pf: PartialFunction[AnyRef, Activation]): Disposable

  /**
   * registers a new extension point.
   */
  def register[A <: Extension](extensionPoint: A): RegisteredExtensionPoint[A]


  def activateWith[A](f: A => Activation)(implicit manifest: ClassManifest[A]): Disposable

  /**
   * Applies the function to all matching extension points and return the results.
   */
  def map[A](pf: PartialFunction[Extension, A]): Iterable[A]
}

/**
 * some generic trait to dispose any resources associated with it
 */
trait Disposable {

  /**
   * release any resources.
   */
  def dispose: Unit
}

/**
 * optional result of an applied function in ExtensionContext#activate which #deactivate function is called when the extension point is unregister.
 */
trait Activation extends Disposable {
}

/**
 * Result of a registered activation through ExtensionContext#activate. Allows unregistering the activation registration
 */
trait Registration extends Disposable {
}

/**
 * defines a registered extension point
 */
trait RegisteredExtensionPoint[A] extends Registration {

  /**
   * @return the extension point which is registered
   */
  def extensionPoint: A
}

/**
 * defines an extension point
 */
trait ExtensionPoint extends Disposable {
}

class DefaultExtensionContext extends ExtensionContext {

  type ExtensionPoint = AnyRef

  case class ActiveActivation(extension: AnyRef,
                              pf: PartialFunction[AnyRef, Activation],
                              activation: Activation) extends Activation {

    def dispose = activation.dispose
  }

  var registeredActivations: List[PartialFunction[AnyRef, Activation]] = Nil

  var extensionPoints: List[AnyRef] = Nil

  var activations: List[ActiveActivation] = Nil

  def activationCandidate(extension: AnyRef, pf: PartialFunction[AnyRef, Activation]): Option[Activation] = {
    if (pf.isDefinedAt(extension)) {
      val activation = pf(extension)
      activations = activations :+ ActiveActivation(extension, pf, activation)
      Some(activation)
    } else {
      None
    }
  }

  def register[A <: AnyRef](extension: A) = {
    // go through all existing activations
    for (activate <- registeredActivations) {
      activationCandidate(extension, activate)
    }

    // keep the extension point
    extensionPoints = extensionPoints :+ extension
    new RegisteredExtensionPoint[A] {
      override def dispose = {
        extensionPoints = extensionPoints.filterNot(_ == extension)
        for (activation <- activations; if activation.extension == extension) {
          activation.dispose
        }
        activations = activations.filterNot(_.extension == extension)
      }

      def extensionPoint = extension
    }
  }

  def activate(pf: PartialFunction[AnyRef, Activation]): Disposable = {
    for (extension <- extensionPoints) {
      activationCandidate(extension, pf)
    }
    registeredActivations = registeredActivations :+ pf
    dispose {
      for (activation <- activations; if activation.pf == pf) {
        activation.dispose
      }
      activations = activations.filterNot(_.pf == pf)
    }
  }

  def activateWith[A](f: (A) => Activation)(implicit manifest: ClassManifest[A]) = null

  def register[A](pf: PartialFunction[AnyRef, A]) = {
    new {
      def apply: Iterable[A] = {
        for (extension <- extensionPoints.toStream; if pf.isDefinedAt(extension)) yield {
          pf(extension)
        }
      }
    }
  }

  def map[A](pf: PartialFunction[ExtensionPoint, A]): Iterable[A] = {
    for {
      extension <- extensionPoints.toStream
      if pf.isDefinedAt(extension)
    } yield {
      pf(extension)
    }
  }
}
