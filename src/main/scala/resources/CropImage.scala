package org.mbr.imagery.resources

import org.mbr.imagery.page.PageContent
import javax.ws.rs.{PathParam, GET, Path}
import javax.ws.rs.core.{Response, UriBuilder}

/**
 * User: mathias
 * Date: 21.03.11 23:36
 * Time: 23:36
 */
@Path("/crop")
class CropImageResource extends PageContent {

  @GET
  @Path("{path:.*}")
  def show(@PathParam("path") pathToImage: String): Response = {
    null
  }
}

trait CropImage {
}