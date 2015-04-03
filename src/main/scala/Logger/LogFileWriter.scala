package Logger

import java.io.{File, PrintWriter}

/**
 * Created by erisa on 03/04/15.
 */
object LogFileWriter {

def writeToLog(logPath: String, logEntry: Entry): Unit ={
  val writer = new PrintWriter(new File(logPath))
}

}
