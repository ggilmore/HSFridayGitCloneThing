/**
 * Created by gmgilmore on 4/3/15.
 */



import java.io.File
import Errors.{FileCopyFailedException, GenError}
import Logger.LogFileReader._
import Logger.LogFileWriter._
import Logger.Entry

import Util.Util._
import org.apache.commons.io.FileUtils

import scala.io.Source


object ArgParser extends App {

  val SNAPSHOT_FOLDER_NAME = ".myvcs/"

  val USAGE =s"""USAGE 'snapshot' followed by the path of the folder that you want the snapshot to be placed."""
  val LOG_FILENAME = "myvcs_log.txt"

  args match {
    case Array("snapshot", targetFolder) => commit(targetFolder = targetFolder)
    case Array("snapshot", "-m", message, targetFolder) => commit(message, targetFolder)
    case Array("checkout", version, repoFolder) => ???
    case _ => println(USAGE)
  }


  def commit(message:String = "", targetFolder:String) = {
    val logLines = Source.fromFile(new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME)).getLines.toSeq.filter(x=>x.nonEmpty)
    getLatestRepositoryEntry(logLines) match {
      case Some(entry) => createNewSnapShot((entry.version.toInt +1).toString, "",
        CURRENT_RUNNING_PATH, targetFolder, new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
      case None => createNewSnapShot(0.toString, "",
        CURRENT_RUNNING_PATH, targetFolder, new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
   }
  }

  private def copyAllFiles(rootPath: String, targetDirName: String, snapVersion:String): Option[GenError] = {
    try {
      val folder = new File(rootPath)
      val target = new File(targetDirName, SNAPSHOT_FOLDER_NAME + snapVersion)
      FileUtils.copyDirectory(folder, target)
      None
    }
    catch {
      case e: java.io.IOException => Some(FileCopyFailedException( s"""Unable to copy contents of $rootPath to ${rootPath + targetDirName}"""))

    }

  }

  def createNewSnapShot(newVersion:String, message:String ="", rootPath:String, targetDirName:String, logPath:String) = {
    writeToRepositoryLog(logPath, Entry(newVersion, message))
    copyAllFiles(rootPath, targetDirName, newVersion)
  }


}


