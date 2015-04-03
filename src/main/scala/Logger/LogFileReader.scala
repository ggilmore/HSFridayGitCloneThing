package Logger

/**
 * Created by erisa on 03/04/15.
 */
object LogFileReader {

  def readRepositoryLine(line: String) ={
    val trimmedLine = line.trim
    val firstSpace = trimmedLine.indexOf(" ")
    Entry(trimmedLine.substring(0, firstSpace), trimmedLine.substring(firstSpace + 1))
  }

  def getRepositoryEntries(lines: Seq[String]):Seq[Entry] = {
    def loop(entries:Seq[Entry], restOfLines:Seq[String]): Seq[Entry] = {
      if (restOfLines.isEmpty) entries
      else loop(entries :+ readRepositoryLine(restOfLines.head), restOfLines.tail)
    }
    loop(Seq(), lines)
  }

  def getLatestRepositoryEntry(lines:Seq[String]):Option[Entry] = {
    try {
      val entries = getRepositoryEntries(lines)
      Some(entries(entries.size -1))
    }catch {
      case e: IndexOutOfBoundsException => None
    }
  }

  def getBranches(baseLogLines: Seq[String]):Seq[RepositoryInfo] = {
    def loop(entries:Seq[RepositoryInfo], restOfLines:Seq[String]): Seq[RepositoryInfo] = {
      if (restOfLines.isEmpty) entries
      else loop(entries :+ readBaseLogLine(restOfLines.head), restOfLines.tail)
    }
    loop(Seq(), baseLogLines)
  }

  def readBaseLogLine(line:String):RepositoryInfo = {
    val trimmedLine = line.trim
    val firstSpace = trimmedLine.indexOf(" ")
    RepositoryInfo(trimmedLine.substring(0, firstSpace), trimmedLine.substring(firstSpace + 1))
  }

  def getBranchPath(baseLogLines:Seq[String], branchName:String):Option[String] = {
    val nameLocMap = getBranches(baseLogLines).foldLeft(Map[String, String]()){case (map, entry) => map + (entry.name -> entry.location)}
    nameLocMap.get(branchName)
  }


}
