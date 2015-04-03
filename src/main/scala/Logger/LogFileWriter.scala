package Logger

import java.io.{File, PrintWriter}

/**
 * Created by erisa on 03/04/15.
 */
object LogFileWriter {

  def writeToRepositoryLog(logPath: String, logEntry: Entry): Unit ={
    val writer = new PrintWriter(new File(logPath))
    writer.append(s"""${logEntry.version} ${logEntry.message}""")
    writer.flush
    writer.close
  }

  def updateBaseLog(logPath:String, branch:RepositoryInfo) = {
    val writer = new PrintWriter(new File(logPath))
    writer.append(s"""${branch.name} ${branch.location}""")
    writer.flush
    writer.close
  }

}
