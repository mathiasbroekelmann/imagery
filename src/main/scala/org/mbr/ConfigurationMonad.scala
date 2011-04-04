package org.mbr.extension

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

object Extension {

  /**
   * create an extension reader to resolve something from the extension context.
   */
  def collectFirst[A](pf: PartialFunction[AnyRef, A]): Extension[A] = {
    new Extension[A] {
      def apply(context: ExtensionContext) = context.collectFirst(pf)
    }
  }

  /**
   * create an extension reader to resolve something from the extension context.
   */
  def firstOf[A](implicit manifest: ClassManifest[A]): Extension[A] = {
    collectFirst {
      case x if manifest.erasure.isInstance(x) => {
        x.asInstanceOf[A]
      }
    }
  }

  def collect[A](pf: PartialFunction[AnyRef, A]): Extensions[A] = {
    new Extensions[A] {
      def apply(context: ExtensionContext) = context.collect(pf)
    }
  }
}

trait Extension[A] {

  self =>

  /**
   * monad implementation
   */
  def apply(context: ExtensionContext): Option[A]

  def map[B](f: A => B): Extension[B] = {
    new Extension[B] {
      def apply(c: ExtensionContext) = {
        for (some <- self.apply(c)) yield {
          f(some)
        }
      }
    }
  }

  def flatMap[B](f: A => Extension[B]): Extension[B] = {
    new Extension[B] {
      def apply(c: ExtensionContext) = {
        self.apply(c).flatMap(some => f(some).apply(c))
      }
    }
  }
}

trait Extensions[A] {

  self =>

  /**
   * monad implementation
   */
  def apply(context: ExtensionContext): Iterable[A]

  def map[B](f: A => B): Extensions[B] = {
    new Extensions[B] {
      def apply(c: ExtensionContext) = {
        for (some <- self.apply(c)) yield {
          f(some)
        }
      }
    }
  }

  def flatMap[B](f: A => Extensions[B]): Extensions[B] = {
    new Extensions[B] {
      def apply(c: ExtensionContext) = {
        self.apply(c).flatMap(some => f(some).apply(c))
      }
    }
  }
}

/**
 * extension context provides access to registered extensions.
 */
trait ExtensionContext {

  def collectFirst[A](pf: PartialFunction[AnyRef, A]): Option[A] = collect(pf).headOption

  /**
   * Returns extensions which can be applied to the given function
   */
  def collect[A](pf: PartialFunction[AnyRef, A]): Iterable[A]
}

/**
 * allows registration of extensions.
 */
trait ExtensionRegistry {
  /**
   * registers an extension which may have dependencies to other extensions
   * creates a new extension contexts
   */
  def register[A](extension: Extension[A]): ExtensionRegistry

  /**
   * registers an extension without any dependencies to other extensions.
   */
  def register(extension: AnyRef): ExtensionRegistry

  /**
   * 
   */
  def build: ExtensionContext
}

import Extension._

class SomeExtensionReaderTest {

  def whatever = {

    val tree: Extension[Tree] = collectFirst {case tree: Tree => tree}
    val apple: Extension[Apple] = firstOf[Apple]

    val user = collectFirst {case user: User => user}

    val trees: Extensions[Tree] = collect {case tree: Tree => tree}

    val bean: Extension[BeanWithDependencies] = for {
      t <- tree
      a <- apple
      u <- user
    } yield {
      BeanWithDependencies(t, a, u)
    }

    val context: ExtensionContext = null
    context.register(bean)
    val contextTrees: Iterable[Tree] = trees.apply(context)
    val maybeBean: Option[BeanWithDependencies] = bean.apply(context)
    val treeOption: Option[Tree] = for (b <- bean.apply(context)) yield {
      b.tree
    }
  }

}

trait Tree

trait Apple

trait User

case class BeanWithDependencies(tree: Tree, apple: Apple, user: User)