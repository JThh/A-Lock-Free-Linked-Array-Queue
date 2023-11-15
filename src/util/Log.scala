package ox.cads.util

import java.io._
import scala.collection.mutable.ArrayBuffer

/** Classes to perform various types of logging.
  *
  * The recommended use of any code using a logger log is 
  * try{ ... log("whatever") ... }{finally log.shutdown }
  */

trait Log{
  /** Log message msg */
  def apply(msg: => String) : Unit
 
  /** Shut-down the log cleanly */
  def shutdown : Unit

  /** An assertion, which cleanly shuts down the log if false. */
  def assert(ok: Boolean, st: => String) : Unit = 
    if(!ok){
      apply("***ERROR*** "+st); shutdown; sys.error(st+"\n"); sys.exit()
    }

  /** An assertion, which cleanly shuts down the log if false. */
  def assert(ok: Boolean) : Unit = this.assert(ok: Boolean, "")
  
}

// ==================================================================

/** A logging object that does nothing with its arguments */
final object NullLog extends Log{
  /** Log message msg */
  @inline def apply(msg: => String) : Unit = {}
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = {}

  /** An assertion, which cleanly shuts down the log if false. */
  override def assert(ok: Boolean, st: => String) : Unit =
    if(!ok){
      val st1 = st; val fname = "/tmp/error"+ThreadID.get
      val thisLog = new FileLog(fname)
      // java.lang.Throwable.printStackTrace()
      val stackTrace = new Exception().getStackTrace.mkString("\n")
      thisLog(st1); thisLog(stackTrace); thisLog.shutdown;
      println(stackTrace+"\n"+"Error written to "+fname)
      sys.error(st1+"\n"); sys.exit()
    }
}

// ==================================================================

/** A logging object that just prints to the screen */
final object ScreenLog extends Log{
  /** Log message msg */
  @inline def apply(msg: => String) : Unit = println(msg)
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = {}
}

// ==================================================================

/** A logging object that writes to a file
  * @param fname the name of the file to use for the log */
final class FileLog(fname: String) extends Log{
  private val writer = new PrintWriter(fname)

  /** Log message msg */
  @inline def apply(msg: => String) : Unit = writer.write(msg+"\n")
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = { writer.flush(); writer.close() }
}

// ==================================================================

/** A logging object that stores data internally, and outputs to a file when
  * shut down. */
final class ArrayBufferLog(fname: String) extends Log{
  private val ab = new ArrayBuffer[String]()

  /** Log message msg */
  @inline def apply(msg: => String) : Unit = {
    val st = msg // evaluate msg outside the mutex
    synchronized{ ab += (st) }
  }

  /** Shut-down the log, writing output to file. */
  def shutdown : Unit = {
    val writer = new PrintWriter(fname)
    for(st <- ab) writer.write(st+"\n")
    writer.flush(); writer.close()
  }

}

// ==================================================================

/** A logging object, that stores data internally in thread-local logs,
  * indexed by timestamps, and outputs to a file when shut down. */
final class TSLog(fname: String, p: Int) extends Log{
  private val logs = Array.fill(p)(new TSThreadLog)

  def apply(t: Int, msg: String) : Unit = logs(t).log(msg)

  def apply(msg: => String) : Unit = apply(ThreadID.get%p, msg)

  def shutdown : Unit = {
    val totalLog = TS.merge(logs.map(_.get))
    val writer = new PrintWriter(fname)
    for(st <- totalLog) writer.write(st+"\n")
    writer.flush(); writer.close()
  }
}

/** A logging object for a single thread, using timestamps. */
final class TSThreadLog{
  /** ArrayBufer storing the messages. */
  private val theLog = new ArrayBuffer[TS[String]]

  /** Add msg to the log. */
  def log(msg: String) = theLog += new TS(msg)

  /** Get the log. */
  def get : Array[TS[String]] = theLog.toArray

}
