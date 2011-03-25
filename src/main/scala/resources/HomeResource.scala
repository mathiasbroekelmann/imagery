package org.mbr.imagery.resources

import javax.ws.rs._

import core._
import core.Response.{ResponseBuilder, Status}
import org.imagemagick.{Gravity, Color, Geometry, Convert}
import com.sun.jersey.api.view.{Viewable, ImplicitProduces}
import org.mbr.imagery.page.{Defaults, PageContent}
import org.mbr.imagery.sidebar.{SidebarElement, Sidebar}
import java.util.Date
import image.ResourceCache
import java.net.{URI}
import Convert._
import java.io.{InputStream, FileFilter, OutputStream, File}
import org.mbr.imagery.image.{Image, StoredImage, ImageStore}
import org.mbr.imagery.blob.FileBlob

/**
 * The root resource bean
 */
@Path("/album")
class HomeResource extends PageContent with Album with Defaults {

  val parent = None

  val path = "/"

  def storeUri = URI.create("/")

  def location = new File("/media/fotos")
}

object Album {
  val thumbnailType = "png"
  val lightboxType = "jpg"
}

trait ImageCropping extends ImageStore {

  self =>

  @Path("{imageName}/crop")
  def crop(@PathParam("imageName") imageName: String) = {
    load(URI.create(imageName)) match {
      case Some(image) => new CropImage(self, image)
      case _ => throw NotFoundException(imageName)
    }
  }
}

/**
 * sub resource of an album
 */
@ImplicitProduces(Array("text/html;qs=5"))
trait Album extends SidebarElement with ImageStore with ImageCropping {

  self =>

  type Element = Album

  type Store = Album

  type Image = ImageTeaser

  import Album._

  def link = UriBuilder.fromResource(classOf[HomeResource]).path(storeUri.toString).build().toString

  protected def nestedImageStore(name: String) = NestedAlbum(self, name)

  protected def storedFileImage(file: File, storeUri: URI): ImageTeaser = {
    val absoluteUri = UriBuilder.fromUri(self.storeUri).path(storeUri.toString).build()
    FileImageTeaser(file, absoluteUri)
  }

  @Path("{album}")
  def album(@PathParam("album") album: String) = {
    childStore(album) match {
      case Some(store) => store
      case _ => throw NotFoundException(album)
    }
  }

  @GET
  def album = new Viewable("index", self, self.getClass)

  def derivedImageResponse[A](imageName: String,
                              request: Request,
                              derivation: String,
                              format: String,
                              create: (InputStream, OutputStream) => A): ResponseBuilder = {
    println("requesting imageName: " + imageName)

    def found(storedImage: StoredImage) = {

      println("found imageName: " + storedImage)

      def respondWithContent = {
        val response = Response.ok(new StreamingOutput {
          def write(out: OutputStream) = {
            ResourceCache.cached(storedImage, derivation) {
              cachedOut => storedImage.read(create(_, cachedOut))
            } write (out)
          }
        }, "imageName/" + format)

        storedImage.lastModified match {
          case Some(time) => response.lastModified(new Date(time))
          case _ => response
        }
      }

      storedImage.lastModified match {
        case Some(time) => Option(request.evaluatePreconditions(new Date(time))).getOrElse(respondWithContent)
        case _ => respondWithContent
      }
    }

    load(URI.create(imageName)) match {
      case Some(image) => found(image)
      case _ => Response.status(Status.NOT_FOUND)
    }
  }

  @GET
  @Path("{name}-thumbnail.{extension}")
  def thumbnail(@PathParam("name") name: String,
                @Context request: Request): Response = {

    def createThumbnail(in: InputStream, out: OutputStream) = {
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
        .writeAs(thumbnailType).to(out)
    }

    derivedImageResponse(name, request, "thumbnail", thumbnailType, createThumbnail).build
  }

  @GET
  @Path("{name}-lightbox.{extension}")
  def lightbox(@PathParam("name") name: String,
               @Context request: Request) = {

    def createLightbox(in: InputStream, out: OutputStream) = {
      val width = 1027
      val height = 768
      val size = Geometry(width, height)

      convert.define(jpegSize(Geometry(width * 2, height * 2)))
        .apply(image(in).buffered)
        .autoOrient
        .thumbnail(size)
        .unsharp(0, .5)
        .writeAs(lightboxType).to(out)
    }

    derivedImageResponse(name, request, "lightbox", lightboxType, createLightbox).build
  }

  override def pictures: Iterable[ImageTeaser] = {
    super.pictures.toSeq.sortBy(_.lastModified.getOrElse(0L))
  }

  def children = childStores.toSeq.sortBy(_.name)
}

case class NestedAlbum(someParent: Album,
                       override val name: String)
  extends PageContent with Album with Defaults {

  lazy val parent = Some(someParent)

  lazy val storeUri = UriBuilder.fromUri(someParent.storeUri).path(name).build()

  lazy val location = new File(someParent.location, name)
}

case class NotFoundException(message: String) extends WebApplicationException(Status.NOT_FOUND)

case class FileImageTeaser(file: File, storeUri: URI) extends FileBlob with ImageTeaser {

  def uriByTeaser(teaser: String): String = {
    UriBuilder.fromResource(classOf[HomeResource]).path(storeUri + "-" + teaser).build().toString
  }

  lazy val thumbnail = new HtmlImage {
    def src = uriByTeaser("thumbnail." + Album.thumbnailType)
  }

  lazy val lightbox = new HtmlImage {
    def src = uriByTeaser("lightbox." + Album.lightboxType)
  }
}

/**
 * defines an imageName shown in the web page
 */
trait ImageTeaser extends StoredImage {

  /**
   * the thumbnail imageName
   */
  def thumbnail: HtmlImage

  /**
   * the lightbox imageName
   */
  def lightbox: HtmlImage
}

/**
 * an imageName which is rendered as an html img tag
 */
trait HtmlImage {

  /**
   * src attribute value of the html imageName
   */
  def src: String
}