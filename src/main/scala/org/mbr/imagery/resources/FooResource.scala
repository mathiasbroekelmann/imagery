package org.mbr.imagery.resources

import javax.ws.rs._

import com.sun.jersey.api.view.ImplicitProduces

@Path("/foo")
@ImplicitProduces(Array("text/html;qs=5"))
class FooResource {
}