/**
* A lock-based implementation of linked array queue (note their differences - mainly for comparison purposes).
* Authored by Jiatong Han on Nov 15, 2023. Libraries are available from https://courses.cs.ox.ac.uk/pluginfile.php/7345/course/section/967/ox-cads-lib.2.13.zip
* No distribution without permission.
*/


import scala.reflect.ClassTag

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicReference

import ox.cads.locks.TTASLock 


class LinkedArrayList[T: ClassTag] extends ox.cads.collection.Queue[T]{

    private val size = 2 // number of pieces of data in each node of the
    // linked list
    private class Node {
        val data = new Array[T](size)
        @volatile var nextEnqPos = 0
        var nextDeqPos = 0
        @volatile var next: Node = null
    }

    private val firstNode = new Node // initial dummy header 
    private var head = firstNode
    @volatile private var tail = head

    private val enqLock = new TTASLock()
    private val deqLock = new TTASLock()

    /** Add value to the queue */
    def enqueue(value: T) : Unit = {
        enqLock.lock
        try {
            if (tail != head && tail.nextEnqPos < size) { // current tail not full
                tail.data(tail.nextEnqPos) = value 
                tail.nextEnqPos += 1
            } else { // create a new node and update tail
                var e = new Node
                e.data(e.nextEnqPos) = value
                e.nextEnqPos += 1
                tail.next = e
                tail = e
            }
        } finally {
            enqLock.unlock
        }
    }

    /** Dequeue and return a value if the queue is non-empty; else return null */
    def dequeue : Option[T] = {
        var result: Option[T] = None
        deqLock.lock
        try {
            var done = false
            var prevNode = head
            var curNode = prevNode.next
            while (curNode != null && !done) {
                if (curNode.nextDeqPos < curNode.nextEnqPos) {
                    result = Some(curNode.data(curNode.nextDeqPos))
                    curNode.nextDeqPos += 1
                    done = true
                    if (curNode.nextDeqPos >= size) {
                        if (curNode != tail) 
                            {prevNode.next = curNode.next}  
                        // drop the node if current node is not tail to avoid conflicts.
                    }
                }
                prevNode = curNode
                curNode = curNode.next
            }
        } finally {
            deqLock.unlock
        }
        result
    }
}