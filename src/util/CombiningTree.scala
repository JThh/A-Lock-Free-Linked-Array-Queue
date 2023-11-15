package ox.cads.util
import scala.collection.mutable.Stack

/** A counter based on a combining tree.
  * Based on TAoMP, Section 12.3.
  * @param width the number of threads that will use this.
  */
class CombiningTree(width: Int){
  private val nodes = new Array[Node](width-1)
  nodes(0) = new Node
  for(i <- 1 until width-1) nodes(i) = new Node(nodes((i-1)/2))
  private val leafs = Array.tabulate((width+1)/2)(i => nodes(width-2-i))

  /** Increment the counter, and return its previous value. */
  def getAndIncrement : Int = {
    val myLeaf = leafs(ThreadID.get/2)
    var node = myLeaf
    // precombining phase
    while(node.precombine) node = node.parent
    val stop = node
    // combining phase 
    node = myLeaf; var combined = 1; val stack = new Stack[Node]
    while(node != stop){
      combined = node.combine(combined); stack.push(node); node = node.parent
    }
    // operation phase
    val result =
      if(stop.isRoot) stop.op(combined)
      else stop.store(combined)
    // distribution phase
    while(stack.nonEmpty){ node = stack.pop; node.distribute(result) }
    result
  }

  // -------- The Node class --------

  /** Nodes in the combining tree. */
  private class Node(val parent: Node){
    // Node statuses
    private val Root = -1; private val Idle = 0; private val First = 1
    private val Second = 2; private val Solo = 3; private val SecondStored = 4
    private val Result = 5
    private var status = Idle // status of this node
    private var firstValue, secondValue = -1 // values deposited by threads
    private var result = 0 // result of this op/state, for the root only

    /** Constructor for the root node. */
    def this() = { this(null); status = Root }

    /** Is this the root node? */
    def isRoot = status == Root

    /** Precombining; result indicates whether thread should continue. */
    def precombine : Boolean = synchronized{
      while(status != Idle && status != First && status != Root) wait()
      status match{
	case Idle => { status = First; true }
	case First => { status = Second; false }
	case Root => false
      }
    }

    /** Store value, and wait for result. */
    def store(value: Int) : Int = synchronized{
      assert(status == Second)
      secondValue = value // deposit my value
      status = SecondStored; notify() // wake up other thread
      waitForResult
    }

    /** Wait for the result, and return it. */
    private def waitForResult : Int = synchronized{
      while(status != Result) wait()
      status = Idle; notify() // wake up thread on next round
      result
    }

    /** Combining. */
    def combine(combined: Int) : Int = synchronized{
      while(status == Second) wait()
      firstValue = combined // deposit my value
      status match{
	case First => { status = Solo; combined }
	case SecondStored => 
	  combined + secondValue // combine with other thread's value
      }
    }

    /** Perform operation on the root node. */
    def op(combined: Int) : Int = synchronized{
      assert(status == Root); val prior = result; result += combined; prior
    }
 
    /** Distribution. */
    def distribute(prior: Int) = synchronized{
      status match{
	case Solo => status = Idle
	case SecondStored => { result = prior + firstValue; status = Result }
      }
      notify()
    }
  }
}
