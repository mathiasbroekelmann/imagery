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
import sbt._
import org.fusesource.scalate.sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) 
  extends DefaultWebProject(info) 
  with PrecompilerWebProject 
  with Eclipsify {
  lazy val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  lazy val fusesource_snapshot_repo = "FuseSource Snapshots" at
           "http://repo.fusesource.com/nexus/content/repositories/snapshots"
  lazy val java_net_repo = "Java.net Repository" at
           "http://download.java.net/maven/2"
  
  lazy val commonsIo        = "commons-io" % "commons-io" % "1.4"

  lazy val scalate_guice    = "org.fusesource.scalate" % "scalate-guice"     % "1.4.0" 
  lazy val servlet          = "javax.servlet"          % "servlet-api"       % "2.5" 
  lazy val logback          = "ch.qos.logback"         % "logback-classic"   % "0.9.26"

  // to get jetty-run working in sbt
  lazy val jetty_webapp     = "org.eclipse.jetty"      % "jetty-webapp"     % "7.0.2.RC0" % "test"
  lazy val scalateTest      = "org.fusesource.scalate" % "scalate-test"     % "1.4.0" % "test"

  lazy val imageMetadata    = "com.kenai.nbpwr" % "com-drew-metadata" % "2.4.0-beta-1" from "http://www.drewnoakes.com/drewnoakes.com/code/exif/releases/metadata-extractor-2.4.0-beta-1.jar"

  lazy val jodaDateTime     = "joda-time" % "joda-time" % "1.6.2" withSources

  lazy val squeryl = "org.squeryl" % "squeryl_2.8.1" % "0.9.4-RC3"

  lazy val postgresql = "postgresql" % "postgresql" % "8.4-702.jdbc4"

  lazy val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7" % "test"

  lazy val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"

  lazy val mockito = "org.mockito" % "mockito-all" % "1.8.5" % "test"
}
