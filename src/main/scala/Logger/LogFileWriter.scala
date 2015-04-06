package Logger

import java.io.{FileWriter, PrintWriter}

/**
 * Created by erisa on 03/04/15.
 */
object LogFileWriter {

  val DELIMETER = "#!"

  def updateBaseLog(logPath:String, entryInfo:Entry) = {
    val writer = new PrintWriter(new FileWriter(logPath, true))
    writer.println(
      s"""${entryInfo.version}$DELIMETER${entryInfo.message}
         |$DELIMETER${entryInfo.date}$DELIMETER${entryInfo.parent}""".stripMargin)
    writer.flush
    writer.close
  }

  def writeHeadFile(filePath:String, version:String) = {
    val writer = new PrintWriter(new FileWriter(filePath))
    writer.println(s"""$version""")
    writer.flush
    writer.close
  }

}
