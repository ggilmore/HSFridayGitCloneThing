package Logger

/**
 * Created by erisa on 03/04/15.
 */
object LogFileReader {

  def readLine(line: String) ={
    val trimmedLine = line.trim
    val firstSpace = trimmedLine.indexOf(" ")
    Entry(trimmedLine.substring(0, firstSpace), trimmedLine.substring(firstSpace + 1))
  }

  def getLatestEntry(lines: Seq[String]) = {
    readLine(lines.head)
  }
}
