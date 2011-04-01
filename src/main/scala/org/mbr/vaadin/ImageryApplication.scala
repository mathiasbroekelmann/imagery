package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}
import Extension._

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
        val boundAction = topNav.register(Action("FooBar", tooltip = Some("Tooltip of FooBar"))).click {
          println("FooBar clicked")
        }
        dispose(boundAction.unbind)
      }
    }
  }
}

/**
 * User: mathias
 * Date: 27.03.11 16:14
 * Time: 16:14
 */
class ImageryApplication extends VaadinApplication {

  self =>

  var activatedExtensions: Iterable[Disposable] = Nil

  def init {
    val extensionContext = new DefaultExtensionContext
    val frame = new DefaultApplicationFrame(self) with Extension with ExtensionPoint {
      frame =>
      def start(context: ExtensionContext) = {
        self.setMainWindow(window)
        self.setTheme("imagery")
        context.register(frame)
      }

      def dispose = self.removeWindow(window)
    }

    val modules = frame :: ImageryApplication.extensions
    activatedExtensions = (for (extension <- modules) yield {
      extension.start(extensionContext)
    })
  }

  override def close = {
    for (activation <- activatedExtensions) {
      activation.dispose
    }
    activatedExtensions = Nil
  }
}
