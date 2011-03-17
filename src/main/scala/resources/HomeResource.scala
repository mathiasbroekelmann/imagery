/**
 * Copyright (C) 2009-2011 the original author or authors.
 * See the notice.md file distributed with this work for additional
 * information regarding copyright ownership.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.mbr.imagery.resources

import javax.ws.rs._

import core._
import core.Response.Status
import java.net.URI
import org.mbr.imagery.image.Image
import org.mbr.imagery.blob.{UriBlob}
import java.util.Date
import org.imagemagick.{Gravity, Color, Geometry, Convert}
import com.sun.jersey.api.view.{Viewable, ImplicitProduces}
import org.mbr.imagery.page.{Defaults, PageContent}
import org.mbr.imagery.sidebar.{SidebarElement, Sidebar}
import java.io.{FileFilter, OutputStream, File}

/**
 * The root resource bean
 */
@Path("/")
class HomeResource extends PageContent with Album with Defaults {

  def directory = new File("/home/mathias/Bilder/Fotos")

  @Context
  val uriInfo: UriInfo = uriInfo

  val parent = None

  val path = "/"
}

object Album {
  val thumbnailType = "png"
}

/**
 * sub resource of an album
 */
@ImplicitProduces(Array("text/html;qs=5"))
trait Album extends SidebarElement {

  self =>

  type Element = Album

  import Album._

  def path: String

  def directory: File

  def uriInfo: UriInfo

  def name: String = directory.getName

  @Path("{album}")
  def album(@PathParam("album") album: String) = {
    val albumDir = new File(directory, album)
    if (albumDir.exists) {
      new NestedAlbum(Some(self), albumDir, uriInfo)
    } else {
      throw AlbumNotFoundException(album)
    }
  }

  @GET
  def album = new Viewable("index", self, self.getClass)

  @GET
  @Path("{name}-thumbnail.png")
  def thumbnail(@PathParam("name") name: String,
                @Context request: Request) = {
    val file = new File(directory, name)
    val width, height = 150
    if (!file.exists) {
      Response.status(Status.NOT_FOUND).build
    } else {
      val lastModified = new Date(file.lastModified)
      Option(request.evaluatePreconditions(lastModified)).getOrElse {
        Response.ok(new StreamingOutput {
          def write(out: OutputStream) = {
            import Convert._
            val size = Geometry(width, height)
            convert(image(file))
              .autoOrient
              .thumbnail(Geometry(width * height).area)
              .background(Color.transparent)
              .gravity(Gravity.Center)
              .extent(size)
              .unsharp(0, .5)
              .writeAs(thumbnailType).to(out)
          }
        }, "image/" + thumbnailType).lastModified(new Date(file.lastModified))
      }.build
    }
  }

  lazy val dashboard = new Dashboard {
    val location = directory
  }

  def pictures: Iterable[ImageTeaser] = {

    val baseLocation = directory.toURI.toString

    def asteaser(uri: URI): ImageTeaser = new ImageTeaser {

      val baseRelativeLocation = uri.toString.stripPrefix(baseLocation)

      def uriByTeaser(teaser: String): URI = {
        UriBuilder.fromPath(path).path(baseRelativeLocation + "-" + teaser).build()
      }

      def original = new HtmlImage {
        def src = uriByTeaser("original.jpg")
      }

      def popup = new HtmlImage {
        def src = uriByTeaser("popup.jpg")
      }

      def thumbnail = new HtmlImage {
        def src = uriByTeaser("thumbnail." + thumbnailType)
      }
    }

    dashboard.pictures(asteaser)
  }

  def children = {
    def childDirs = Option(directory.listFiles(new FileFilter {
      def accept(pathname: File) = pathname.isDirectory
    })).map(_.toStream).getOrElse(Stream.empty)

    for (dir <- childDirs) yield {
      new NestedAlbum(Some(self), dir, uriInfo)
    }
  }
}

class NestedAlbum(val parent: Option[Album],
                  val directory: File,
                  val uriInfo: UriInfo)
  extends PageContent with Album with Defaults {

  def path = parent match {
    case Some(album) => if (album.path.endsWith("/")) album.path + directory.getName else album.path + "/" + directory.getName
    case None => "/" + directory.getName
  }
}

case class AlbumNotFoundException(message: String) extends WebApplicationException(Status.NOT_FOUND)

/**
 * defines an image shown in the web page
 */
trait ImageTeaser {

  /**
   * the numbnail of the image
   */
  def thumbnail: HtmlImage

  /**
   * the popup image
   */
  def popup: HtmlImage

  /**
   * the original image
   */
  def original: HtmlImage
}

/**
 * an image which is rendered as an html img tag
 */
trait HtmlImage {

  /**
   * src attribute value of the html image
   */
  def src: URI
}