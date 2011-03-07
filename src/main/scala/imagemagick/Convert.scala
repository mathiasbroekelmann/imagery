package org.imagemagick

import java.io.{InputStream, File}

/**
 * User: mathias
 * Date: 07.03.11 20:32
 * Time: 20:32
 */

trait Setup extends ImageSettings with ImageSourceSpec

trait ImageCommands extends Commands with ImageSettings

trait Convert extends Setup {

  /**
   * apply image conversion to the given image file
   */
  def apply(file: File) = image(file)

  /**
   * apply image conversion to the given input image file defined as path
   */
  def apply(file: String) = image(file)

  /**
   * apply image conversion to the given input image file defined as path
   */
  def apply(in: InputStream) = image(in)
}

object Convert extends Convert {

  def apply(setting: ImageSetting) = apply(List(setting))

  def apply(settings: Iterable[ImageSetting] = Nil): Convert = new Convert {

    def apply(setting: ImageSetting) = Convert.apply(settings ++ List(setting))

    type Settings = Convert

    def commands = settings
  }

  type Settings = Convert

  def commands = Nil
}