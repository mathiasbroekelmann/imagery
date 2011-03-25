package org.mbr.imagery.resources

import org.mbr.imagery.page.PageContent
import javax.ws.rs.{PathParam, GET, Path}
import javax.ws.rs.core.{Response, UriBuilder}
import org.mbr.imagery.image.Image
import com.sun.jersey.api.view.Viewable
import org.mbr.imagery.sidebar.SidebarElement

/**
 * User: mathias
 * Date: 21.03.11 23:36
 * Time: 23:36
 */
class CropImage(val origin: AnyRef, image: Image) extends PageContent {

  @GET
  def cropImage = new Viewable("index", this, getClass)
}

case class CroppableImage(image: Image) {

  def uriByTeaser(teaser: String): String = {
    UriBuilder.fromResource(classOf[HomeResource]).path(image.uri + "-" + teaser).build().toString
  }

  lazy val thumbnail = new HtmlImage {
    def src = uriByTeaser("thumbnail." + Album.thumbnailType)
  }

  lazy val lightbox = new HtmlImage {
    def src = uriByTeaser("lightbox." + Album.lightboxType)
  }
}