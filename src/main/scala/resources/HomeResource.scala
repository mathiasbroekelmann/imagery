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

import com.sun.jersey.api.view.ImplicitProduces
import core._
import core.Response.ResponseBuilder
import java.net.URI
import org.mbr.imagery.image.Image
import java.io.{OutputStream, File}
import org.apache.commons.io.IOUtils
import org.mbr.imagery.blob.{FileBlob, UriBlob}
import image.ImageResizer

/**
 * The root resource bean
 */
@Path("/")
@Produces(Array("text/html;qs=5"))
class HomeResource {

  def base = new File("/media/fotos/2010")

  @Context
  val uriInfo: UriInfo = uriInfo

  lazy val dashboard = new Dashboard {
    val location = base
  }

  case class HtmlImage(location: URI, val width: Option[Int] = None, val height: Option[Int] = None)
    extends Image {

    val nestedSrc = location.toString.stripPrefix(base.toURI.toString)

    def withWidth(w: Int) = new HtmlImage(location, Some(w), height)

    def withHeight(h: Int) = new HtmlImage(location, width, Some(h))

    def src = {
      val builder = UriBuilder.fromPath(uriInfo.getPath).path("/pic/{src}")
      val wb = width match {
        case Some(w) => builder.queryParam("w", w.asInstanceOf[Object])
        case _ => builder
      }
      val hb = height match {
        case Some(h) => builder.queryParam("h", h.asInstanceOf[Object])
        case _ => builder
      }
      hb.buildFromEncoded(nestedSrc)
    }

    def loc = location

    def blob = new UriBlob {}
  }

  def pictures: Iterable[HtmlImage] = {
    dashboard.pictures(HtmlImage(_))
  }

  def picture(imageFile: File,
              width: Option[Int],
              height: Option[Int]): Response = {
    if (imageFile.exists) {
      Response.ok(new StreamingOutput {
        def write(out: OutputStream) = {
          val image = new FileBlob {
            def file = imageFile
          }.read(ImageResizer.resize(_, width, height))
        }
      }).build
    } else {
      Response.status(404).build
    }
  }
  @GET
  @Path("pic/{src:.*}")
  @Produces(Array("image/*"))
  def picture(@PathParam("src") src: String,
              @QueryParam("w") width: Int = 0,
              @QueryParam("h") height: Int = 0): Response = {
    picture(new File(base, src), if(width > 0) Some(width) else None, if(height > 0) Some(height) else None)
  }

}

trait HtmlImage extends Image {

  def loc: URI

  def src: URI

  def blob = new UriBlob {
    override lazy val uri = loc
  }
}