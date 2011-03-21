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
import org.imagemagick.{Gravity, Color, Geometry, Convert}
import com.sun.jersey.api.view.{Viewable, ImplicitProduces}
import org.mbr.imagery.page.{Defaults, PageContent}
import org.mbr.imagery.sidebar.{SidebarElement, Sidebar}
import java.io.{FileFilter, OutputStream, File}
import java.util.Date
import image.ResourceCache
import java.net.{URI}
import Convert._

/**
 * The root resource bean
 */
@Path("/")
class HomeResource extends PageContent with Album with Defaults {

  def directory = new File("/media/fotos")

  val parent = None

  val path = "/"
}

object Album {
  val thumbnailType = "png"
  val lightboxType = "jpg"
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

  def link = UriBuilder.fromResource(classOf[HomeResource]).path(path).build().toString

  def directory: File

  def name: String = directory.getName

  @Path("{album}")
  def album(@PathParam("album") album: String) = {
    val albumDir = new File(directory, album)
    if (albumDir.exists) {
      NestedAlbum(Some(self), albumDir)
    } else {
      throw AlbumNotFoundException(album)
    }
  }

  @GET
  def album = new Viewable("index", self, self.getClass)

  @GET
  @Path("{name}-thumbnail.{extension}")
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
            ResourceCache.cached(file.toURI.toURL, "thumbnail") {
              cachedOut =>
                val size = Geometry(width, height)
                convert.define(jpegSize(Geometry(width * 4, height * 4)))
                  .apply(image(file))
                  .autoOrient
                  .thumbnail(Geometry(width * 2 * height * 2).area)
                  .borderColor(Color("snow"))
                  .border(Geometry(5, 5))
                  .background(Color("black"))
                  .polaroid(0)
                  .resize(Geometry(50.0))
                  .writeAs(thumbnailType).to(cachedOut)
            } write (out)
          }
        }, "image/" + thumbnailType).lastModified(new Date(file.lastModified))
      }.build
    }
  }

  @GET
  @Path("{name}-lightbox.{extension}")
  def lightbox(@PathParam("name") name: String,
               @Context request: Request) = {
    val file = new File(directory, name)
    val width = 1027
    val height = 768
    if (!file.exists) {
      Response.status(Status.NOT_FOUND).build
    } else {
      val lastModified = new Date(file.lastModified)
      Option(request.evaluatePreconditions(lastModified)).getOrElse {
        Response.ok(new StreamingOutput {
          def write(out: OutputStream) = {
            ResourceCache.cached(file.toURI.toURL, "lightbox") {
              cachedOut =>
                val size = Geometry(width, height)
                convert.define(jpegSize(Geometry(width * 2, height * 2)))
                  .apply(image (file))
                  .autoOrient
                  .thumbnail(size)
                  .unsharp(0, .5)
                  .writeAs(lightboxType).to(cachedOut)
            } write (out)
          }
        }, "image/" + lightboxType).lastModified(new Date(file.lastModified))
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

      def uriByTeaser(teaser: String): String = {
        UriBuilder.fromResource(classOf[HomeResource]).path(path).path(baseRelativeLocation + "-" + teaser).build().toString
      }

      lazy val original = new HtmlImage {
        def src = uriByTeaser("original.jpg")
      }

      lazy val thumbnail = new HtmlImage {
        def src = uriByTeaser("thumbnail." + thumbnailType)
      }

      lazy val lightbox = new HtmlImage {
        def src = uriByTeaser("lightbox." + lightboxType)
      }

      lazy val lastModified: Date = new Date(uri.toURL.openConnection.getLastModified)
    }

    dashboard.pictures(asteaser).toSeq.sortBy(_.lastModified.getTime)
  }

  def children = {
    def childDirs = Option(directory.listFiles(new FileFilter {
      def accept(pathname: File) = pathname.isDirectory && !pathname.getName.startsWith(".")
    })).map(_.toStream).getOrElse(Stream.empty)

    for (dir <- childDirs.sortBy(_.getName)) yield {
      NestedAlbum(Some(self), dir)
    }
  }
}

case class NestedAlbum(parent: Option[Album],
                       directory: File)
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
   * the thumbnail image
   */
  def thumbnail: HtmlImage

  /**
   * the lightbox image
   */
  def lightbox: HtmlImage

  /**
   * the original image
   */
  def original: HtmlImage

  /**
   * the last modified date
   */
  def lastModified: Date
}

/**
 * an image which is rendered as an html img tag
 */
trait HtmlImage {

  /**
   * src attribute value of the html image
   */
  def src: String
}