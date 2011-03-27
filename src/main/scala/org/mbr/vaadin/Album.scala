package org.mbr.vaadin

import org.mbr.imagery.image.Image
import scala.math._
import com.vaadin.{Application => VaadinApplication}
import com.vaadin.ui._

object Modules {
  def extensions: Iterable[Extension] = TopNavigation :: Nil
}

/**
 * User: mathias
 * Date: 27.03.11 16:14
 * Time: 16:14
 */
class ImageryApplication extends VaadinApplication with ApplicationFrame {

  self =>

  val container = new CssLayout
  val window = new Window("Imagery", container)
  val top = new CssLayout
  val sidebar = new CssLayout
  val marginal = new CssLayout
  val content = new CssLayout
  val footer = new CssLayout

  def init {
    setMainWindow(window)
    setTheme("imagery")
    container.addStyleName("container")
    buildApplicationFrame
  }

  def buildApplicationFrame = {
    top.addStyleName("top")
    container.addComponent(top)
    top.addComponent(new Label((<h1>Header</h1>).toString, Label.CONTENT_XHTML))

    sidebar.addStyleName("sidebar")
    container.addComponent(sidebar)
    sidebar.addComponent(new Label((<h3>Sidebar</h3>).toString, Label.CONTENT_XHTML))

    marginal.addStyleName("marginal")
    container.addComponent(marginal)
    marginal.addComponent(new Label((<h3>Marginal</h3>).toString, Label.CONTENT_XHTML))

    content.addStyleName("content")
    container.addComponent(content)
    content.addComponent(new Label((<h2>Content</h2>).toString, Label.CONTENT_XHTML))

    footer.addStyleName("footer")
    container.addComponent(footer)
    footer.addComponent(new Label("Footer"))

    val extensionContext = new ExtensionContext {

      case class ActiveActivation(extension: ExtensionPoint,
                                  pf: PartialFunction[ExtensionPoint, Option[Activation]],
                                  activation: Activation) extends Activation {
        def deactivate = activation.deactivate
      }

      def application = self

      var registeredActivations: List[PartialFunction[ExtensionPoint, Option[Activation]]] = Nil

      var extensionPoints: List[ExtensionPoint] = Nil

      var activations: List[ActiveActivation] = Nil

      def activationCandidate(extension: ExtensionPoint, pf: PartialFunction[ExtensionPoint, Option[Activation]]): Option[Activation] = {
        if(pf.isDefinedAt(extension)) {
          for(activation <- pf(extension)) yield {
            activations = activations :+ ActiveActivation(extension, pf, activation)
            activation
          }
        } else {
          None
        }
      }

      def register(extension: ExtensionPoint) = {
        // go through all existing activations
        for(activate <- registeredActivations) {
          activationCandidate(extension, activate)
        }

        // keep the extension point
        extensionPoints = extensionPoints :+ extension
        new RegisteredExtensionPoint {
          def unregister = {
            extensionPoints = extensionPoints.filterNot(_ == extension)
            for(activation <- activations; if activation.extension == extension) {
              activation.deactivate
            }
            activations = activations.filterNot(_.extension == extension)
          }

          def extensionPoint = extensionPoint

          def application = self.application
        }
      }

      def activate(pf: PartialFunction[ExtensionPoint, Option[Activation]]): RegisteredActivation = {
        for(extension <- extensionPoints) {
          activationCandidate(extension, pf)
        }
        registeredActivations = registeredActivations :+ pf

        new RegisteredActivation {
          def unregister = {
            for(activation <- activations; if activation.pf == pf) {
              activation.deactivate
            }
            activations = activations.filterNot(_.pf == pf)
          }
        }
      }
    }

    for(extension <- Modules.extensions) {
      extension.init(extensionContext)
    }
  }

  def application = self
}

/**
 * define the main application components
 */
trait ApplicationFrame {

  def application: VaadinApplication

  /**
   * the main application window
   */
  def window: Window

  /**
   * the top component container
   */
  def top: ComponentContainer

  /**
   * the left sidebar container
   */
  def sidebar: ComponentContainer

  /**
   * the right marginal container
   */
  def marginal: ComponentContainer

  /**
   * the main content container
   */
  def content: ComponentContainer

  /**
   * the footer container
   */
  def footer: ComponentContainer
}


trait Navigation {

  /**
   * @return the root menu elements
   */
  def menu: Iterable[Menu]
}

trait Menu {
  def id: String
}

trait Plugin

/**
 * a album panel contains the hierarchical tree view of the albums
 */
trait AlbumPanel

/**
 * a preview panel contains a set of image teasers.
 */
trait PreviewPanel {

  /**
   * show the teasers for the given images
   */
  def showImages(images: Iterable[Image]): Pageable[ImageTeaser]
}

/**
 * a teaser which contains the image and actions for the image
 */
trait ImageTeaser

/**
 * a navigation mixin trait to navigate through pages
 */
trait PageableNavigation {

  type Page

  /**
   * navigate to some page
   */
  def goto: {
    def first: Page
    def last: Page
    def next: Option[Page]
    def previous: Option[Page]
    def apply(pageIndex: Int): Option[Page]
  }
}