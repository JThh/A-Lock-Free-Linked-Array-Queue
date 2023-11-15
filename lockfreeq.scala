/**
* A lock-free implementation of linked array queue.
* Authored by Jiatong Han on Nov 15, 2023. Libraries are available from https://courses.cs.ox.ac.uk/pluginfile.php/7345/course/section/967/ox-cads-lib.2.13.zip
* No distribution without permission.
*/

import scala.reflect.ClassTag

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.concurrent.atomic.AtomicReference

import ox.cads.util.ThreadID

class LFLinkedArrayList[T: ClassTag] extends ox.cads.collection.Queue[T]{

    private val size = 10 // number of pieces of data in each node
    private class Node() {
        val data = new AtomicReferenceArray[Any](size) // Actual data of type T stored in the node array
        var next = new AtomicReference[Node](null) // Reference to next node in the linked array
        val nextEnqPos = new AtomicInteger(0) // Next enqueuing position (inclusive)
        val nextDeqPos = new AtomicInteger(0) // Next dequeuing position (inclusive)
    }

    private val firstNode = new Node // Initial dummy header 
    private val head = new AtomicReference(firstNode)
    private var tail = new AtomicReference(firstNode)

    private val DELETED = AnyRef // Flag for completed deletion (to place in the array)

    /** Add value to the queue */
    def enqueue(value: T) : Unit = {
        var done = false
        while (!done) {
            val myTail = tail.get; val next = myTail.next.get
            var index = myTail.nextEnqPos.get
            if (index < size) {
                while (!done && (index < size)) {
                    if (myTail.data.compareAndSet(index, null.asInstanceOf[T], value)) {
                        myTail.nextEnqPos.set(index+1); done = true
                        // Using set here might cause slight slower update than the actual enqueue position.
                        // But since we have distinguished the deleted element, non-null element, and null, 
                        // it will still be fine (with a few more CAS checks).
                    } 
                    index += 1
                } 
            }

            if (!done && index >= size) { // insert new node; previous tail is full
                if (myTail == tail.get) {
                    if (next == null) {
                        var node = new Node
                        node.data.set(node.nextEnqPos.getAndIncrement, value)
                        if (myTail.next.compareAndSet(next, node)){ // lin. pt. if successful
                            tail.compareAndSet(myTail, node); done = true
                        }
                    }
                }
                else { // tail has yet been updated, so we need to insert the value to next
                    tail.compareAndSet(myTail, next) // other threads may help
                }
            }
        }
    }

    /** Dequeue and return a value if the queue is non-empty; else return null */
    def dequeue : Option[T] = {
        var done = false; var result: Option[T] = None
        while (!done) {
            var myHead = head.get
            var index = myHead.nextDeqPos.get
            if (index < size) {
                while (!done && index < size) {
                    var temp = myHead.data.get(index)
                    if (temp == null) {done = true} // Empty queue
                    else if ((temp != DELETED) &&  // Linearization point (line below).
                            myHead.data.compareAndSet(index, temp, DELETED)) {
                        result = Some(temp.asInstanceOf[T]); done = true 
                        myHead.nextDeqPos.set(index+1)
                        // Using set here might cause slight slower update than the actual enqueue position.
                        // But since we have distinguished the deleted element, non-null element, and null, 
                        // it will still be fine (with a few more CAS checks).
                    }
                    else index += 1
                }
            } 
            if (index >= size) {
                if (myHead.next.get != null) head.compareAndSet(myHead, myHead.next.get)
                else {done = true} // Empty queue
            }
        }
        result
    }
}