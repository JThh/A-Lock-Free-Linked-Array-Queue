## A Lock-Free Implementation of Linked Array Queue

Linked lists with too many nodes introduce a bottleneck of memory access as the node contents are normally stored in a  non-consecutive manner. One way to get this around is to enlarge node size - let each node contain an array that can hold multiple elements, in which way the elements can be saved in a more compact memory space for faster traversal. 

A lock-free implementation of a concurrent linked-list queue requires careful handling of simultaneous push and pop operations with some atomic operations such as `compareAndSet`. When using a linked array, the idea is similar, but we need to care more about maintaining the linearizability of writing and reading from array operations. In my simple lock-free implementation, there is no starvation issue involved since when an arbitrary thread gets descheduled, the other threads will proceed with their operations as normal. 

A test may be carried out using the [linearizability test script](./test.scala) which simply compares the outcomes of a sequential queue with our concurrent queue implementation with many threads. 

This idea was originated in a course work from the University of Oxford. 

### Simple Instructions to Test

In command line:
```
scalac lockfreeq.scala
scalac test.scala

scala SimpleQueueTest --lockFree --reps 1000 --iters 1000 -p 5
```