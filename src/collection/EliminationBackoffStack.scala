package ox.cads.collection

import scala.util.Random
import java.util.concurrent.atomic.AtomicReference

/** A partial stack, based on elimination.  Based on Herlihy & Shavit, 
  * Sections 11.3-5.
  * @param capacity the number of slots in the elimination array; the
  * recommended value is the number of threads, if the stack is going to be 
  * used intensively at times. */
class EliminationBackoffStack[T](capacity: Int, timeout: Long = 64)
extends TotalStack[T]{
  private val eliminationArray = 
    Array.fill(capacity)(new LockFreeExchanger[Option[T]])

  // We'll build linked lists from the following type of Nodes
  private class Node(val value: T){
    var next: Node = null
  }

  // Atomic reference to the top of the stack
  private val top = new AtomicReference[Node](null)

  /** A class of policy objects, which decide the slot to use in the 
    * elimination array, and the timeout to use. */ 
  private class Policy{
    private val random = new scala.util.Random

    /** Size of range from which to choose slot. */
    private var slotRange = capacity/2

    /** Counter based on number of non collisions. */
    private var counter = 0

    // Thresholds on counter at which to expand or shrink the range
    private val ExpandThreshold = 5
    private val ShrinkThreshold = -5

    /** Current timeout to use. */
    private var myTimeout = timeout
    private val MaxTimeout = (timeout*16) max 10000 
    private val MinTimeout = 1

    /** Get the slot in the elimination array to use. */
    def getSlot: Int = random.nextInt(slotRange)

    /** Get the timeout to use in the elimination array. */
    def getTimeout : Long = myTimeout
    
    /** Record that this thread successfully eliminated. */
    def recordElimination = {
      counter -= 1 
      if(counter <= ShrinkThreshold){
	slotRange = (slotRange/2) max 1
	myTimeout = (myTimeout*2) min MaxTimeout
	counter = 0
      }
    }

    /** Record that the thread failed to achieve elimination. */
    def recordNonCollision = { 
      counter += 1 
      if(counter >= ExpandThreshold){
	slotRange = (slotRange*2) min capacity
	myTimeout = (myTimeout/2) max MinTimeout
	counter = 0
      }
    }

  }

  private object policy extends ThreadLocal[Policy]{
    override def initialValue = new Policy
  }

  
  /** Push value onto the stack */
  def push(value: T) = {
    val node = new Node(value)
    val myPolicy = policy.get
    var done = false
    do{
      val oldTop = top.get
      node.next = oldTop
      // try to add node to the stack
      done = top.compareAndSet(oldTop, node) 
      if(!done){ // now try using the elimination array
	val slot = myPolicy.getSlot // Random.nextInt(capacity)
	eliminationArray(slot).exchange(Some(value), myPolicy.getTimeout) match{
	  case Some(None) => { myPolicy.recordElimination; done = true }
	  case Some(Some(v)) => { } // retry
	  case None => myPolicy.recordNonCollision //  retry
	}
      }
    } while(!done)
  }

  /** Pop a value from the stack, waiting until a value is obtained. */ 
  def pop : Option[T] = {
    val myPolicy = policy.get
    while(true){
      val oldTop = top.get
      if(oldTop == null) return None
      else{
	val newTop = oldTop.next
	// try to remove oldTop from list
	if(top.compareAndSet(oldTop, newTop)) return Some(oldTop.value)
      }
      // now try using the elimination array
      val slot = myPolicy.getSlot
      eliminationArray(slot).exchange(None, myPolicy.getTimeout) match{
	case Some(Some(v)) => { myPolicy.recordElimination; return Some(v) }
	case Some(None) => { } // retry
	case None => myPolicy.recordNonCollision // retry
      }
    }
    sys.error("unreachable")
  }
}
