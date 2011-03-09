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

  type HasSource = HasImageSource

  def commands: Iterable[HasCommands]

  def apply(setting: ImageSetting) = SomeRequiresImageSource(commands ++ (setting :: Nil))

  def apply(image: ImageSource) = SomeHasImageSource(commands ++ (image :: Nil))
}

case class SomeHasImageSource(commands: Iterable[HasCommands]) extends Commands with HasImageSource {

  type HasSource = HasImageSource

  override def apply(setup: HasCommands) = SomeHasImageSource(commands ++ (setup :: Nil))
}

trait Setup extends ImageSettings with ImageSourceSpec

trait ImageCommands


object Convert extends RequiresImageSource {
  def commands = Nil
}

case class SomeRequiresImageSource(commands: Iterable[HasCommands]) extends RequiresImageSource