package org.mbr.vaadin

import com.vaadin.{Application => VaadinApplication}
import com.vaadin.ui.Button._
import com.vaadin.event.FieldEvents
import com.vaadin.event.FieldEvents.{BlurListener, BlurEvent, BlurNotifier}
import java.awt.event.ComponentListener
import com.vaadin.ui.Component.{Listener, Event}
import com.vaadin.ui.Window.CloseListener
import com.vaadin.ui._
import java.net.URI
import com.vaadin.data.Container.Hierarchical
import scala.collection.JavaConversions._
import reflect.{BeanProperty, BeanInfo}
import com.vaadin.data.Container
import com.vaadin.data.util.{FilesystemContainer, BeanItem, HierarchicalContainer, BeanItemContainer}
import java.lang.{String, Class}
import org.mbr.imagery.image.{Image, StoredFileImage, StoredImage, ImageStore}
import org.mbr.imagery.blob.FileBlob
import Filesystem._
import org.apache.commons.io.IOUtils
import org.apache.commons.io.output.ByteArrayOutputStream
import com.vaadin.terminal.{Resource, StreamResource, Sizeable}
import image.ResourceCache
import java.io._
import org.imagemagick.Convert._
import org.imagemagick.{Convert, Color, Geometry}
import com.vaadin.terminal.StreamResource.StreamSource

/**
 * @author mathias.broekelmann
 * @since 25.03.11 14:16
 */
class SampleApplication extends VaadinApplication {

  application =>

  import Vaadin._

  val mainWindow = new Window("Hello Matze!")
  val mainPanel = new HorizontalSplitPanel
  val imagesPanel = new CssLayout
  val urifu = new UriFragmentUtility

  def init {
    buildMainLayout
  }

  def buildMainLayout = {
    setMainWindow(mainWindow)
    setTheme("imagery")
    val layout = new CssLayout
    mainWindow.setContent(layout)
    layout.setSizeFull
    layout.setMargin(false)
    mainPanel.setFirstComponent(createAlbumTree)
    mainPanel.setSecondComponent(imagesPanel)

    mainPanel.setSplitPosition(200, Sizeable.UNITS_PIXELS)
    mainWindow.addComponent(mainPanel)
    mainWindow.addComponent(urifu)
    mainPanel.setSizeFull
    imagesPanel.setSizeFull
    urifu.setSizeUndefined
  }

  def createAlbumTree = {
    val filter = new FilenameFilter {
      def accept(dir: File, name: String) = new File(dir, name).isDirectory && !name.startsWith(".")
    }
    val albumRoot = new File("/media/fotos")
    val store = new FilesystemContainer(albumRoot, filter, true)
    val tree = new Tree("Alben", store)
    tree.addStyleName("album-nav")
    tree.setItemCaptionPropertyId(FilesystemContainer.PROPERTY_NAME)
    tree.setItemCaptionMode(AbstractSelect.ITEM_CAPTION_MODE_PROPERTY)
    tree.setImmediate(true)
    println("initial fragment path: " + urifu.getFragment)

    /**
     * expand album tree up to the given directory
     */
    def expand(album: File) {
      if(album != albumRoot) {
        expand(album.getParentFile)
        tree.expandItem(album)
      }
    }

    urifu.fragment {
      case path => {
        val album = new File(albumRoot, path)
        if(album.exists && album.isDirectory && album.canRead) {
          tree.setValue(album)
          expand(album.getParentFile)
        }
      }
    }
    tree.valuechange {
      case file: File => {
        println("some album: " + file)
        urifu.setFragment(file.getAbsolutePath.stripPrefix(albumRoot.getAbsolutePath))
        showImagesIn(file)
      }
    }
    tree
  }

  def showImagesIn(directory: File) = {
    val images = directory.collect {
      case file: File if file.toURI.toURL.openConnection.getContentType.startsWith("image/") => createImage(file)
    }
    imagesPanel.removeAllComponents
    for(image <- images.take(20)) {
      val teaser: Embedded = new Embedded(null, image.thumbnail)
      teaser.addStyleName("image-teaser")
      imagesPanel.addComponent(teaser)
      teaser.click {
        case Left(_) => println("image: " + image.name + " left clicked")
        case Right(_) => println("image: " + image.name + " right clicked")
        case Middle(_) => println("image: " + image.name + " middle clicked")
      }
    }
  }

  def createImage(imageFile: File) = new Image with FileBlob with Name {

    self =>

    def file = imageFile

    def name = imageFile.getName

    def thumbnail: Resource = {

      def create(in: InputStream, out: OutputStream): Unit = {
        println("render thumbnail")
        val width, height = 150
        val size = Geometry(width, height)
        convert.define(jpegSize(Geometry(width * 4, height * 4)))
          .apply(image(in).buffered)
          .autoOrient
          .thumbnail(Geometry(width * 2 * height * 2).area)
          .borderColor(Color("snow"))
          .border(Geometry(5, 5))
          .background(Color("black"))
          .polaroid(0)
          .resize(Geometry(50.0))
          .writeAs("png").to(out)
      }

      val source = new StreamSource {
        def getStream = {
          println("get stream for " + name)
          val out = new ByteArrayOutputStream
          ResourceCache.cached(self, "thumbnail") {
            cachedOut => self.read(create(_, cachedOut))
          } write out
          new ByteArrayInputStream(out.toByteArray)
        }
      }

      new StreamResource(source, file.getName + "-thumbnail.png", application)
    }

    def getStream = read { input =>
      val out = new ByteArrayOutputStream
      IOUtils.copy(input, out)
      new ByteArrayInputStream(out.toByteArray)
    }.getOrElse(new ByteArrayInputStream(new Array[Byte](0)))
  }
}

trait Name {
  def name: String
}