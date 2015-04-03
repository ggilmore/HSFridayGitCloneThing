package Errors

/**
 * Created by gmgilmore on 4/3/15.
 */
sealed trait GenError {
  val message:String
}

case class FileCopyFailedException(message:String) extends GenError

case class LogFileFormatException(message: String) extends GenError

case class ExistingBranchException (message: String) extends  GenError

case class BranchNotFoundException (message: String) extends  GenError