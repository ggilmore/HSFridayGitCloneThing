package Errors

/**
 * Created by gmgilmore on 4/3/15.
 */
trait GenError {
  val message:String
}

case class FileCopyFailedException(message:String) extends GenError

case class LogFileFormatException(message: String) extends