package ox.cads.util

/** A class implementing exponential backoff.
  * Based on Herlihy & Shavit, Chapter 7.
  * @param minDelay the minimum length of delay, in nanos.
  * @param maxDelay the maximum length of delay, in nanos.
  * @param ratio the ratio by which the maximum backoff is increased on each 
  * iteration.
  * @param profile hitting the limit of maxDelay.
  */ 
class Backoff(minDelay: Int, maxDelay: Int,
              ratio: Float = 2.0F, profile: Boolean = false){
  def this() = this(1, 1 << 20)

  /** The maximum length of the next delay */
  private var limit = minDelay

  /** Perform exponential backoff */
  def apply() = {
    val delay = 1+scala.util.Random.nextInt(limit) // time to delay (nanos)
    if(profile && ratio*limit > maxDelay) Profiler.count("Backoff max")
    limit = (ratio*limit).toInt min maxDelay
    NanoSpin(delay)
    // Thread.sleep(delay)
    // Thread.sleep(delay/1000000, delay%1000000)
  }
  // Note: my experiments suggest that taking delay non-zero gives much better
  // performance; hence the "+1" in the definition of delay.  The two-argument
  // version of Thread.sleep just rounds to the nearest millisecond, so
  // there's not much point in using it.

  /** Reset the limit */
  def reset = limit = minDelay
}
