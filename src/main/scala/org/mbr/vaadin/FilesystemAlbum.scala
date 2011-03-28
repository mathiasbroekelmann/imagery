package org.mbr.vaadin

import com.vaadin.data.util.FilesystemContainer
import Vaadin._
import com.vaadin.ui._
import Filesystem._
import org.mbr.imagery.image.Image
import org.mbr.imagery.blob.FileBlob
import java.io._
import image.ResourceCache
import com.vaadin.terminal.{ExternalResource, DownloadStream}
import org.apache.commons.io.IOUtils
import org.imagemagick.{Convert, Gravity, Color, Geometry}
import Convert._

/**
 * User: mathias
 * Date: 28.03.11 21:25
 * Time: 21:25
 */

object FilesystemAlbum extends Extension {

  val albumRoot = new File("/media/fotos")

  def start(context: ExtensionContext) = {
    context.activate {
      case topNav: TopNavigation => {
        val activation = context.activate {
          case frame: ApplicationFrame => activate(topNav, frame)
        }
        Some(new Activation {
          def deactivate = activation.unregister
        })
      }
    }
    None
  }

  def activate(nav: TopNavigation, frame: ApplicationFrame): Option[Activation] = {
    println("activating filesystem album")

    def showImages(albumDirectory: File): Unit = {
      val fragment = albumFragment(Some(albumDirectory.getAbsolutePath.stripPrefix(albumRoot.getAbsolutePath)))
      frame.urifu.setFragment(fragment)
      showImagesIn(albumDirectory, frame.content)
    }

    val tree = albumtree(showImages)

    val registration = nav.register(Action("Alben")).click {
      frame.sidebar.removeAllComponents
      frame.sidebar.addComponent(tree)
      frame.urifu.setFragment(albumFragment())
    }

    val registeredActions = registerActions(frame, tree)
    Some(new Activation {
      def deactivate = {
        registration.unbind
        registeredActions.deactivate
      }
    })
  }

  def registerActions(frame: ApplicationFrame, albumTree: Tree): Activation = {
    // image request handling
    val thumbnailHandler = frame.window.handle {
      case ThumbnailRequest(path) => {
        createImage(new File(albumRoot, path)).thumbnail.download
      }
    }

    /**
     * expand album tree up to the given directory
     */
    def expand(album: File) {
      if(album != albumRoot && album != null) {
        expand(album.getParentFile)
        albumTree.expandItem(album)
      }
    }

    val fragmentListener = frame.urifu.fragment {
      case AlbumFragment(path) => {
        frame.sidebar.removeAllComponents
        frame.sidebar.addComponent(albumTree)
        val album = new File(albumRoot, path)
        if (album.exists && album.isDirectory && album.canRead) {
          albumTree.setValue(album)
          expand(album.getParentFile)
        }
      }
    }

    new Activation {
      def deactivate = {
        frame.urifu.removeListener(fragmentListener)
        frame.window.unhandle(thumbnailHandler)
      }
    }
  }

  def albumFragment(path: Option[String] = None) = {
    path match {
      case None => "album"
      case Some(path) => "album" + path
    }
  }

  object AlbumFragment {
    def unapply(fragment: String): Option[String] = {
      if(fragment.startsWith("album")) {
        Some(fragment.stripPrefix("album"))
      } else {
        None
      }
    }
  }

  def albumtree(showImages: File => Unit): Tree = {

    val filter = new FilenameFilter {
      def accept(dir: File, name: String) = new File(dir, name).isDirectory && !name.startsWith(".")
    }
    val store = new FilesystemContainer(albumRoot, filter, true)
    val tree = new Tree("Alben", store)
    tree.addStyleName("album-nav")
    tree.setItemCaptionPropertyId(FilesystemContainer.PROPERTY_NAME)
    tree.setItemCaptionMode(AbstractSelect.ITEM_CAPTION_MODE_PROPERTY)
    tree.setImmediate(true)

    /**
     * expand album tree up to the given directory
     */
    def expand(album: File) {
      if (album != albumRoot) {
        expand(album.getParentFile)
        tree.expandItem(album)
      }
    }

    tree.valuechange {
      case file: File => {
        println("some album: " + file)
        showImages(file)
      }
    }
    tree
  }

  def showImagesIn(directory: File, container: ComponentContainer) = {
    val images = directory.collect {
      case file: File if file.toURI.toURL.openConnection.getContentType.startsWith("image/") => createImage(file)
    }
    container.removeAllComponents
    for (image <- images.take(20)) {
      val teaser = new CssLayout
      val thumbnailImage = new Embedded(null, image.thumbnail.resource)
      teaser.addComponent(thumbnailImage)
      teaser.addStyleName("image-teaser")
      container.addComponent(teaser)

      thumbnailImage.setSizeUndefined
      thumbnailImage.click {
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

    def thumbnail = new {

      def download: DownloadStream = {
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
            .background(Color.transparent)
            .gravity(Gravity.South)
            .extent(Geometry(200, 200))
            .writeAs("png").to(out)
        }

        println("get stream for " + name)
        val out = new ByteArrayOutputStream
        ResourceCache.cached(self, "thumbnail") {
          cachedOut => self.read(create(_, cachedOut))
        } write out

        new DownloadStream(new ByteArrayInputStream(out.toByteArray), "image/png", name)
      }

      def resource = {
        new ExternalResource("/image/" + file.relativeTo(albumRoot) + "-thumbnail.png", "image/png"); //source, file.getName + "-thumbnail.png", application)
      }

    }

    def getStream = read {
      input =>
        val out = new ByteArrayOutputStream
        IOUtils.copy(input, out)
        new ByteArrayInputStream(out.toByteArray)
    }.getOrElse(new ByteArrayInputStream(new Array[Byte](0)))
  }
}