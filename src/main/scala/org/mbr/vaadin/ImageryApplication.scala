package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}

/**
 * define the activated extensions for the application
 */
object ImageryApplication {

  def extensions: List[Extension] = {
    TopNavigation :: FooTopNavigationAction :: FilesystemAlbum :: Nil
  }
}

object FooTopNavigationAction extends Extension {

  def start(context: ExtensionContext) = {
    println("FooBar started")
    context.activate {
      case topNav: TopNavigation => {
        println("top navigation activated")
        topNav.register(Action("FooBar", tooltip = Some("Tooltip of FooBar"))).click {
          println("FooBar clicked")
        }
        Some(Activated)
      }
    }
    None
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
    activatedExtensions = (for (extension <- modules) yield {
      extension.start(extensionContext)
    }).flatten
  }

  override def close = {
    for (activation <- activatedExtensions) {
      activation.stop
    }
  }
}
