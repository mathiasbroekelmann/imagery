package org.imagemagick

import org.apache.commons.lang.builder.HashCodeBuilder
import java.io.{File, OutputStream}

/**
 * User: mathias
 * Date: 05.03.11 18:34
 * Time: 18:34
 */

trait Commands {

  /**
   * holds the list of commands to apply.
   */
  def commands: Iterable[HasCommands]

  /**
   * resolves the argument list
   */
  def arguments = (for(c <- commands) yield c.commands).flatten
}
























































/**
 * TODO
 */






/**
 * Used by #alpha function to set a flag on an image indicating whether or not to use existing alpha channel data,
 * to create an alpha channel, or to perform other operations on the alpha channel.
 */








