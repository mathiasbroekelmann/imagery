package org.mbr.imagery.resources

import com.sun.jersey.api.view.ImplicitProduces
import javax.ws.rs.{GET, Path, Produces}

@Path("/sample")
@ImplicitProduces(Array("text/html;qs=5"))
class SampleResource {

  // TODO delete me:
  // this is just some dummy data we can use in a template
  def name = "Mathias"
}
