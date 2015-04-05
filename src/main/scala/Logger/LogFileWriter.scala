package Logger

import java.io.{FileWriter, PrintWriter}

/**
 * Created by erisa on 03/04/15.
 */
object LogFileWriter {

  def writeToRepositoryLog(logPath: String, logEntry: Entry): Unit ={
    val writer = new PrintWriter(new FileWriter(logPath, true))
    writer.println(s"""${logEntry.version} ${logEntry.message}""")
    writer.flush
    writer.close
  }

  def updateBaseLog(logPath:String, branch:RepositoryInfo) = {
    val writer = new PrintWriter(new FileWriter(logPath, true))
    writer.println(s"""${branch.name} ${branch.location}""")
    writer.flush
    writer.close
  }

}
