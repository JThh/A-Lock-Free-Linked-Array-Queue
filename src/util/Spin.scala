package ox.cads.util

/** An object that will just spin for a certain number of iterations.  
  * Note that this is not thread safe. */
object Spin{
  /** Spin for n iterations.
    * To a first approximation, on a typical machine, each iteration takes 
    * about 1ns (post JIT compilation). */
  @noinline def apply(n: Int) : Unit = new Spin(n)()

  // The following variable is used for a horrible hack that seems to prevent
  // compiler/hardware optimizations.  At least, it works on my machine. 
  //private var totalDelay : Long = 0
}

/** An object that will just spin for a certain number of iterations.  
  * Note that this is thread safe provided each thread uses its own object. */
class Spin(n: Int){
  // Writes to the following are intended to prevent compiler optimizations.
  @volatile private var global = 0
  
  /** Spin for n iterations.
    * To a first approximation, on a typical machine, each iteration takes 
    * a few nanoseconds (post JIT compilation); however, optimizations can 
    * have a fairly large effect on the precise time. */
  @noinline def apply() : Unit = {
    //val init = scala.util.Random.nextInt(100000)
    var count = 0
    while(count < n){
      count += 1; global ^= count //  (count*count)
    }
    // synchronized{ global += 1 }
    // Various previous hacks to prevent optimizations.
    //private val mask1 = 0x12345678
    //private val mask2 = 0xABCDEF90
    // count += 1; count ^= mask1; count ^= 0xB9F9B9E8; count ^= mask2
    // count ^= 0x12345678 ; count = count >> 1 ; count ^= 152709948;
    //global += java.lang.System.nanoTime
  }
}
