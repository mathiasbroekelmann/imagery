package org.imagemagick

/**
 * User: mathias
 * Date: 06.03.11 20:43
 * Time: 20:43
 */
/**
 * identifies a media descriptor
 */
trait Media extends Parameter

/**
 * defines a plain media descriptor which only defines the kind of media
 */
case class NamedMedia(media: String) extends Media with Flagable[Media] {

  /**
   * optionally define an offset
   */
  def offset(x: Int, y: Int) = OffsetMedia(media, x, y)

  def apply(x: Int, y: Int) = offset(x, y)

  def build(flag: String, esc: Boolean) = new Media {
    def spec = media + flag
    override def escape = NamedMedia.this.escape || esc
  }

  def spec = media
}

/**
 * defines a media which has an offset definition
 */
case class OffsetMedia(media: String, x: Int, y: Int) extends Media with Flagable[Media] {

  def build(flag: String, esc: Boolean) = new Media {
    def spec = OffsetMedia.this.spec + flag
    override def escape = esc
  }

  def spec = media +
    (if (x >= 0) "+" + x else x.toString) +
    (if (y >= 0) "+" + y else y.toString)
}

/**
 * contains entry points to media definitions
 */
trait MediaDefinition {

  implicit def apply(name: String) = NamedMedia(name)

  /**
   * some common used media
   */
  def Legal = apply("Legal")
  def Letter = apply("Letter")
  def A0 = apply("A0")
  def A1 = apply("A1")
  def A2 = apply("A2")
  def A3 = apply("A3")
  def A4 = apply("A4")
  def A5 = apply("A5")
  def A6 = apply("A6")
}

object Media extends MediaDefinition
