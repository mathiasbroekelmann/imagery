package org.imagemagick

/**
 * User: mathias
 * Date: 07.03.11 21:57
 * Time: 21:57
 */
object FilterType extends Enumeration {
  type FilterType = Value

  val Bartlett, Bessel, Blackman, Bohman, Box, Catrom, Cubic, Gaussian, Hamming, Hanning, Hermite, Kaiser = Value
  val Lagrange, Lanczos, Mitchell, Parzen, Point, Quadratic, Sinc, Triangle, Welsh = Value

}

object Filter {

  /**
   * define the filter type to create a filter instance.
   */
  def apply(filterType: FilterType.FilterType): Filter = new Filter {
    override def commands = filterType.toString :: Nil

    def apply(setting: FilterSetting) = Filter(filterType, setting :: Nil)
  }

  def apply(filterType: FilterType.FilterType, settings: Iterable[FilterSetting]): Filter = new Filter {
    override def commands = filterType.toString :: Nil ++ (for(cmd <- settings) yield cmd.commands).flatten

    def apply(setting: FilterSetting) = Filter(filterType, settings ++ (setting :: Nil))
  }
}

/**
 * defines a filter setting
 */
trait FilterSetting extends HasCommands

/**
 * You can modify how the filter behaves as it scales your image through the use of these expert settings
 */
trait Filter extends HasCommands {

  /**
   * apply the image setting the the list of existing commands.
   * function is used to create the result of the apply function.
   */
  def apply(setting: FilterSetting): Filter

  /**
   * Scale the X axis of the filter (and its window). Use > 1.0 for blurry or < 1.0 for sharp.
   * This should only be used with Gaussian and Gaussian-like filters simple filters, or you may not get the expected results.
   */
  def blur(factor: Double) = apply(new ParameterImageAttribue("define", "filter:blur=" + factor) with FilterSetting)

  /**
   * Set the filter support radius.
   * Defines how large the filter should be and thus directly defines how slow the filtered resampling process is.
   */
  def support(radius: Int) = apply(new ParameterImageAttribue("define", "filter:support=" + radius) with FilterSetting)

  /**
   * Set the number of lobes to use for the Sinc/Bessel filter.
   * This an alternative way of specifying the 'support' range of the filter,
   * that is designed to be more suited to windowed filters, especially when used for image distorts.
   */
  def lobes(count: Int) = apply(new ParameterImageAttribue("define", "filter:lobes=" + count) with FilterSetting)

  def bspline(factor: Double) = apply(new ParameterImageAttribue("define", "filter:b=" + factor) with FilterSetting)

  /**
   * Redefine the values used for cubic filters such as Cubic, Catrom, Mitchel, and Hermite,
   * as well as the Parzen Sinc windowing function. If only one of the values are defined,
   * the other is set so as to generate a 'Keys' type cubic filter.
   */
  def cubic(factor: Double) = apply(new ParameterImageAttribue("define", "filter:c=" + factor) with FilterSetting)

  /**
   * Use this function directly as the scaling filter.
   * This will allow you to directly use a windowing filter such as Blackman,
   * rather than as its normal usage as a windowing function for 'Sinc' or 'Bessel' functions.
   * If defined, no windowing function is used, unless the following expert setting is also defined.
   */
  def filter(function: String) = apply(new ParameterImageAttribue("define", "filter:filter=" + function) with FilterSetting)

  /**
   * The IIR (infinite impulse response) filters Bessel and Sinc are windowed
   * (brought down to zero over the defined support range) with the given filter.
   * This allows you to specify a filter function that is not normally used as a windowing function,
   * such as Box, (which effectively turns off the windowing function), to window a Sinc,
   * or the function the previous setting defined.
   */
  def window(function: String) = apply(new ParameterImageAttribue("define", "filter:window=" + function) with FilterSetting)
}