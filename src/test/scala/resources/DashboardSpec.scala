package org.mbr.imagery.resources

import java.io.File
import org.specs.Specification

/**
 * User: mathias
 * Date: 27.02.11 17:38
 * Time: 17:38
 */

class DashboardSpec extends Specification {

  val base = new File("/media/fotos")

  "images of dashboard " in {
    val dashboard = new Dashboard {
      lazy val location = base
    }
    val pics = dashboard.pictures
    for(pic <- pics.take(20)) {
      println(pic.src)
    }
  }
}