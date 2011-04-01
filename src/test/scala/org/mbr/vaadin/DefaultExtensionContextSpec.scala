package org.mbr.vaadin

import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack
import org.scalatest.{Spec, AbstractSuite, FlatSpec}
import Extension._

/**
 * @author mathias.broekelmann
 * @since 31.03.11 14:18
 */
class DefaultExtensionContextSpec extends Spec with ShouldMatchers {

  describe("A ExtensionContext") {

    val context = new DefaultExtensionContext
    val extension = new TestExtension

    it("register/activating an extension point") {
      val registration = context.register(extension)
      registration.extensionPoint should equal(extension)
      var activated: Boolean = false

      val activation: Disposable = context.activate {
        case x: TestExtension => {
          activated = true
          dispose(activated = false)
        }
      }
      activated should equal(true)
      registration.dispose
      activated should equal(false)
    }
  }
}

class TestExtension {

  override def toString = "TestExtension"
}