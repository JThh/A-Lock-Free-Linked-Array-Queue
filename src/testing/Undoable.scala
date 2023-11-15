package ox.cads.testing

/** Classes that mix in the `Undoable` class provide an operation
 *  `undo` which can be used to undo the last operation.
 *
 */
trait Undoable {
  /** Undo the last operation.
   */
  def undo(): Unit
}
