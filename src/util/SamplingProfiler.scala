package ox.cads.util

import scala.collection.JavaConverters._  
import java.lang.StackTraceElement
import scala.collection.mutable.ArrayBuffer

/** A sampling profiler.
  * 
  * @param interval the interval (in ms) between samples.
  * @param print a function to produce the output; by default, 
  * SamplingProfiler.print */
class SamplingProfiler(
  interval: Int = 20, 
  print: ArrayBuffer[SamplingProfiler.StackTraces] => String =
    SamplingProfiler.print(_)
){
  /** Has the profiler been signalled to finish? */
  private var done_ = false

  /** Indicate to the profiler that it should finish, because the profiled code
    * has finished. */
  def done = done_ = true

  private type StackTraces = SamplingProfiler.StackTraces

  /** A string representing the output of the profile. */
  private var output = ""

  /** Run the profile until done is called. */
  def run() = {
    // Observed StackTraces are stored in the following
    val store = new ArrayBuffer[StackTraces]
    output = ""

    // Monitor the stacktraces
    while(!done_){
      val stackTraces : StackTraces = Thread.getAllStackTraces()
      store += stackTraces
      Thread.sleep(interval)
    }

    output = print(store) // SamplingProfiler.print(store, length, filter)
  }

  /** Run the computation comp in parallel with this profiler.
    * @param print should the output be printed?
    * @return the output.
    */
  def apply(comp: => Unit, print: Boolean = true) : String = {
    ThreadUtil.runParallel(this.run, {comp; done})
    if(print) println(output)
    output
  }

}

// ==================================================================

object SamplingProfiler{
  private type StackTraces =
    java.util.Map[Thread, Array[java.lang.StackTraceElement]]

  /** String representing the stacktraces in store.
    * @param length the number of entries to output.
    * @param filter a filtering function to decide if a given StackTraceElement 
    * should be included. */
  def print(store: ArrayBuffer[StackTraces], length: Int = 20,
            filter: StackTraceElement => Boolean = SamplingProfiler.notAPIFrame)
      : String = {
    // Extract information from store into map from (class name, method name)
    // to elapsed time with this as the top (non-filtered) frame, or anywhere
    // in the stack.
    //val thisThread = java.lang.Thread.currentThread
    val table =
      new scala.collection.mutable.HashMap[(String, String), (Int, Int)]
    for(stackTraces <- store;
        //  thread <- stackTraces.keySet.iterator.asScala; if thread != thisThread){
        stackTrace <- stackTraces.values.iterator.asScala){
      // val stackTrace = stackTraces.get(thread)
      val filteredTrace = stackTrace.filter(filter).map(getKey)
      if(filteredTrace.nonEmpty){
        // Top frame in stack
        val key = filteredTrace(0)
        table.get(key) match{
          case Some((n1,n2)) => table.put(key, (n1+1,n2+1))
          case None => table.put(key, (1,1))
        }
        // Remaining frames
        for(i <- 1 until filteredTrace.length){
          val key = filteredTrace(i)
          // test if this is the first occurrence of key
          var found = false
          for(j <- 0 until i; if !found) found = filteredTrace(j) == key
          if(!found) table.get(key) match{
            case Some((n1,n2)) => table.put(key, (n1,n2+1))
            case None => table.put(key, (0,1))
          }
        }
      }
    }

    // Produce output
    val sortedTable1 = table.toArray.sortBy(_._2._1).takeRight(length).reverse
    val sortedTable2 = table.toArray.sortBy(_._2._2).takeRight(length).reverse
    val sortedTable = sortedTable1++sortedTable2
    val nameColWidth = // width of name column
      (sortedTable.map{case ((c,m),_) => c.length+m.length}.max + 2) min 70
    val topColWidth = sortedTable.map(_._2._1).max.toString.length + 1
    def showEntry(entry: ((String, String), (Int, Int))) : String = {
      val ((c,m),(n1,n2)) = entry; val name = c+"."+m
      name.takeRight(nameColWidth) + " "*(nameColWidth-name.length) +
        n1 + " "*(topColWidth-n1.toString.length) + n2
    }

    sortedTable1.map(showEntry).mkString("\n") + "\n\n" +
      sortedTable2.map(showEntry).mkString("\n")
  }


  /** Extract (class-name, method-name) from stack frame. */
  private def getKey(f: StackTraceElement) = (f.getClassName, f.getMethodName)


  /** Is frame not an API call, i.e. from a class not starting with "scala.",
    * "java." or "ox.cads."?  This is the default filter used when
    * constructing SamplingProfilers. */
  def notAPIFrame(frame: StackTraceElement) : Boolean = {
    val c = frame.getClassName;
    !(c.startsWith("scala.") || c.startsWith("java.") ||
        c.startsWith("ox.cads."))
  }
}
