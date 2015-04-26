/**
 * Created by gmgilmore on 4/3/15.
 */



import java.io.{FileFilter, File}
import Errors._
import Logger.LogFileReader._
import Logger.LogFileWriter._
import Logger.{FileDiffer, LogFileWriter, LogFileReader, Entry}


import Util.Util._
import org.apache.commons.io.FileUtils

object ArgParser extends App {

  val SNAPSHOT_FOLDER_NAME = s".myvcs${File.separator}"

  val USAGE =s"""USAGE 'snapshot' followed by the path of the folder that you want the snapshot to be placed."""
  val LOG_FILENAME = "myvcs_log.txt"
  val BASE_LOG = "base_log.txt"
  val OK = "OK"
  val HEAD_FILE = CURRENT_RUNNING_PATH + "/" + SNAPSHOT_FOLDER_NAME + "/" + "head.txt"
  lazy val BASE_LOG_FILE_PATH = getBaseLogFilePath

  args match {
    case Array("snapshot") => commit() match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }
    case Array("snapshot", "-m", message) => commit(message) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }

    case Array("checkout", "latest") => checkout() match {
      case Some(err) => println(err)
      case None => println(OK)
    }

    case Array("checkout", version) => checkout(Some(version)) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }

    case Array("log") => println(showLog)

//    case Array("diff", original) => println(diff(originalVersion = original))

    case Array("diff", original, revision) => println(diff(original, revision))


    case _ => println(USAGE)
  }

  private def diff(originalVersion:String, revisedVersion:String = "") :String = {
    val revisedVersionFile = new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME + revisedVersion + File.separator)
    if (!revisedVersionFile.exists) s"Revision: $revisedVersion not found at ${revisedVersionFile.getAbsolutePath}"
    else if (revisedVersion.isEmpty){ //is current directory
      FileDiffer.diffTwoDirectories(revisedVersionFile.getAbsolutePath, CURRENT_RUNNING_PATH)
    }
    else{
      val originalVersionFile = new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME + originalVersion + File.separator)
      if (!originalVersionFile.exists) s"Revision: $originalVersion not found at ${originalVersionFile.getAbsolutePath}"
      else {
        FileDiffer.diffTwoDirectories(originalVersionFile.getAbsolutePath, revisedVersionFile.getAbsolutePath)
      }
    }
  }

  private def showLog:String = {
    val builder = new StringBuilder
    LogFileReader.getRepositoryEntries(LogFileReader.getLines(BASE_LOG_FILE_PATH)).foreach{case entry => {
      builder.append(s"""${entry.date} Version: ${entry.version} ParentVersion: ${entry.parent} Message: ${entry.message}""" + "\n")
    }}
    builder.toString
  }

  private def commit(message:String = ""): Option[GenError] = {
    val logLines = LogFileReader.getLines(BASE_LOG_FILE_PATH)
    getLatestRepositoryEntry(logLines) match {
      case Some(entry) => {
        val newVersion = (entry.version.toInt +1).toString
        createNewSnapShot(newVersion, LogFileReader.getCurrentVersion(HEAD_FILE), message,
          CURRENT_RUNNING_PATH, getSnapShotPath(newVersion),new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
      }
      case None => createNewSnapShot(0.toString, LogFileReader.getCurrentVersion(HEAD_FILE),message,
        CURRENT_RUNNING_PATH, getSnapShotPath(0.toString), new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
        None
    }
  }



  private def checkout(version:Option[String] = None): Option[GenError] = {
    val versionToCheckout = version.getOrElse(LogFileReader.getLatestRepositoryEntry(LogFileReader.getLines(BASE_LOG_FILE_PATH)).get.version)
    val versionDir = new File (getSnapShotPath (versionToCheckout) )
    if (versionDir.isDirectory) {
    FileUtils.copyDirectory (versionDir, new File (CURRENT_RUNNING_PATH))
      LogFileWriter.writeHeadFile(HEAD_FILE, versionToCheckout)
      None
    } else Some(VersionNotFoundException(s"version $version not found"))
  }


  private def copyAllFiles(rootPath: String, targetDirName: String, snapVersion:String): Option[GenError] = {
    try {
      val folder = new File(rootPath)
      val target = new File(targetDirName, File.separator)
      val filter = new FileFilter {
        override def accept(pathname: File): Boolean = !pathname.getPath.toLowerCase.contains(SNAPSHOT_FOLDER_NAME)
      }
      FileUtils.copyDirectory(folder, target, filter)
      None
    }
    catch {
      case e: java.io.IOException => Some(FileCopyFailedException( s"""Unable to copy contents of $rootPath to ${rootPath + targetDirName}"""))
    }

  }

  private def createNewSnapShot(newVersion:String, parentVersion:String, message:String ="", rootPath:String, targetDirName:String, logPath:String):Option[GenError] = {
    new File(rootPath, targetDirName).mkdir()
    copyAllFiles(rootPath, targetDirName, newVersion) match {
      case Some(err) => Some(err)
      case None => {
        updateBaseLog(logPath, Entry(newVersion, message, getCurrentDate, parentVersion))
        LogFileWriter.writeHeadFile(HEAD_FILE, newVersion)
        None
      }
    }

  }

  private def getSnapShotPath(version: String) = new File(CURRENT_RUNNING_PATH,SNAPSHOT_FOLDER_NAME + version).getAbsolutePath

  private def getBaseLogFilePath = {
    val logFile = new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME+LOG_FILENAME)
    if (!logFile.exists) {
      logFile.getParentFile.mkdirs()
      logFile.createNewFile
    }
    logFile.getAbsolutePath
  }
}


