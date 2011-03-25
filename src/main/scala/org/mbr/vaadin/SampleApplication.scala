package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}
import com.vaadin.ui.Button._
import com.vaadin.event.FieldEvents
import com.vaadin.event.FieldEvents.{BlurListener, BlurEvent, BlurNotifier}
import java.awt.event.ComponentListener
import com.vaadin.ui.Component.{Listener, Event}
import com.vaadin.ui.Window.CloseListener
import com.vaadin.ui._
import com.vaadin.terminal.Sizeable
import java.io.File
import java.net.URI
import org.mbr.imagery.image.{StoredFileImage, StoredImage, ImageStore}

/**
 * @author mathias.broekelmann
 * @since 25.03.11 14:16
 */
class SampleApplication extends VaadinApplication {

  import Vaadin._

  def init {
    buildMainLayout
  }

  def buildMainLayout = {
    val mainWindow = new Window("Hello Matze!")
    setMainWindow(mainWindow)

    val mainPanel = new HorizontalSplitPanel
    mainPanel.setSizeFull
    mainPanel.setFirstComponent(createAlbumTree)
    mainPanel.setSplitPosition(200, Sizeable.UNITS_PIXELS)
    mainWindow.setContent(mainPanel)
  }

  def createAlbumTree = {
    val tree = new Tree
    val store = new AlbumStore
    for (child <- store.childStores) {
      tree.addItem(child)
    }
    tree
  }
}

class AlbumStore(val parent: Option[AlbumStore] = None, val location: File = new File("/home/APERTO/mathias.broekelmann/Bilder/Fotos")) extends ImageStore {

  val path = "/"

  def storeUri = URI.create("/")

  protected def storedFileImage(file: File, storeUri: URI) = StoredFileImage(file, storeUri)

  protected def nestedImageStore(name: String) = new AlbumStore(Some(this), new File(location, name))

  type Image = StoredImage
  type Store = AlbumStore

  override def toString = name
}