package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}

/**
 * define the activated extensions for the application
 */
object ImageryApplication {

  def extensions: List[Extension] = {
    TopNavigation :: Nil
  }
}

/**
 * User: mathias
 * Date: 27.03.11 16:14
 * Time: 16:14
 */
class ImageryApplication extends VaadinApplication {

  self =>

  var activatedExtensions: Iterable[ActivatedExtension] = Nil

  def init {
    val extensionContext = new DefaultExtensionContext
    val modules = new DefaultApplicationFrame(self) :: ImageryApplication.extensions
    activatedExtensions = for (extension <- modules) yield {
      extension.start(extensionContext)
    }
  }

  override def close = {
    for (activation <- activatedExtensions) {
      activation.stop
    }
  }
}
