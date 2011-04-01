package org.mbr

import org.mbr.vaadin.ExtensionContext

/**
 * @author mathias.broekelmann
 * @since 01.04.11 14:05
 */
class ConfigurationMonad {

  def main(args: Array[String]) {
    // utility construction
    def configReader[A](k: Configuration => A): ConfigReader[A] = {
      new ConfigReader[A] {
        def apply(c: Configuration) = k(c)
      }
    }

    val hostname = configReader(_.hostname)
    val port = configReader(_.port)
    val outfile = configReader(_.outfile)

    val hello =
      for {
        h <- hostname
        p <- port
        o <- outfile
      } yield {
        "Hello there " + h + ":" + p +
          "! Want to write to " + o + "?"
      }

    val conf = Configuration("localhost", 80, "/etc/hosts")

    println(hello(conf))
  }
}

case class Configuration(hostname: String,
                         port: Int,
                         outfile: String)

trait ConfigReader[A] {

  self =>

  def apply(config: Configuration): A

  def map[B](f: A => B): ConfigReader[B] = {
    new ConfigReader[B] {
      def apply(c: Configuration) = {
        f(self.apply(c))
      }
    }
  }

  def flatMap[B](f: A => ConfigReader[B]): ConfigReader[B] = {
    new ConfigReader[B] {
      def apply(c: Configuration) = {
        f(self(c))(c)
      }
    }
  }
}

trait ExtensionReader[A] {

  self =>

  def apply(context: ExtensionContext): A

  def map[B](f: A => B): ExtensionReader[B] = {
    new ExtensionReader[B] {
      def apply(c: ExtensionContext) = {
        f(self.apply(c))
      }
    }
  }

  def flatMap[B](f: A => ExtensionReader[B]): ExtensionReader[B] = {
    new ExtensionReader[B] {
      def apply(c: ExtensionContext) = {
        f(self(c))(c)
      }
    }
  }
}

trait Tree

trait Apple

trait User