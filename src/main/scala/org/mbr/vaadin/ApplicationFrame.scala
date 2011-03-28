package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}
import com.vaadin.ui._

class DefaultApplicationFrame(val application: VaadinApplication) extends Extension with ExtensionPoint with ApplicationFrame {

  self =>

  import application._

  val container = new CssLayout
  val window = new Window("Imagery", container)
  val top = new CssLayout
  val sidebar = new CssLayout
  val marginal = new CssLayout
  val content = new CssLayout
  val footer = new CssLayout
  val urifu = new UriFragmentUtility

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

    container.addComponent(urifu)
  }

  def start(context: ExtensionContext) = {
    setMainWindow(window)
    setTheme("imagery")
    container.addStyleName("container")
    buildApplicationFrame
    context.register(self)
    None
  }
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

  def urifu: UriFragmentUtility
}

