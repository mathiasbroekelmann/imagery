package org.imagemagick

/**
 * User: mathias
 * Date: 07.03.11 20:32
 * Time: 20:32
 */
trait RequiresImageSource extends ImageSettings with ImageSourceSpec {

  /**
   * since an image source is required we stay here until we have an image source
   */
  type Settings = RequiresImageSource

  /**
   * After getting a source we allow definition of commands
   */
  type HasSource = HasImageSource

  def commands: Iterable[HasCommands]

  def apply(setting: ImageSetting) = SomeRequiresImageSource(commands ++ (setting :: Nil))

  def apply(image: ImageSource) = SomeHasImageSource(commands ++ (image :: Nil))
}

case class SomeHasImageSource(commands: Iterable[HasCommands]) extends Commands with HasImageSource with Sugar {

  override def apply(setup: HasCommands) = SomeHasImageSource(commands ++ (setup :: Nil))

}

/**
 * combines different image magic command sequences into a single operation.
 */
trait Sugar extends HasImageSource {

  type HasSource = HasImageSource

  /**
   * Surround the image with a border of color.
   */
  def border(geometry: ImageGeometry, color: Color): Operators = borderColor(color).border(geometry)
}

object Convert extends RequiresImageSource {
  def commands = Nil
  
  def convert = Convert
}

case class SomeRequiresImageSource(commands: Iterable[HasCommands]) extends RequiresImageSource