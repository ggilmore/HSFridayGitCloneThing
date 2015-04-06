/**
 * Created by gmgilmore on 4/3/15.
 */



import java.io.{FileFilter, File}
import java.util
import Errors._
import Logger.LogFileReader._
import Logger.LogFileWriter._
import Logger.{RepositoryInfo, LogFileWriter, LogFileReader, Entry}

import Util.Util._
import org.apache.commons.io.FileUtils

import scala.io.Source


object ArgParser extends App {

  val SNAPSHOT_FOLDER_NAME = s".myvcs${File.separator}"

  val USAGE =s"""USAGE 'snapshot' followed by the path of the folder that you want the snapshot to be placed."""
  val LOG_FILENAME = "myvcs_log.txt"
  val BASE_LOG = "base_log.txt"
  val OK = "OK"
  val HEAD_FILE = CURRENT_RUNNING_PATH + "/" + SNAPSHOT_FOLDER_NAME + "/" + "head.txt"

  args match {
    case Array("create", name, location) => createBranch(name, location) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }
    case Array("snapshot", branchName) => commit(branchName=branchName) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }
    case Array("snapshot", "-m", message, branchName) => commit(message, branchName) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }

    case Array("checkout", "latest", branchName) => checkout(branchName = branchName) match {
      case Some(err) => println(err)
      case None => println(OK)
    }

    case Array("checkout", version, branchName) => checkout(version, branchName) match {
      case Some(err) => println(err.message)
      case None => println(OK)
    }
    case _ => println(USAGE)
  }


  private def commit(message:String = "", branchName:String): Option[GenError] = {
    val logFile = new File(CURRENT_RUNNING_PATH, SNAPSHOT_FOLDER_NAME+LOG_FILENAME)
    if (!logFile.exists) logFile.createNewFile
    val logLines = Source.fromFile(logFile).getLines.toSeq.filter(x=>x.nonEmpty)
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



  private def checkout(version:String = "", branchName:String): Option[GenError] = {
    LogFileReader.getBranchPath(Source.fromFile(new File(CURRENT_RUNNING_PATH, BASE_LOG)).getLines().toSeq.filter(line => line.nonEmpty), branchName) match {
      case Some(targetFolder) => {
        val thisFolder = new File(CURRENT_RUNNING_PATH)
        val branchFolder:Either[Unit, File] = {
          val logFile = new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME)
          if (!logFile.exists) logFile.createNewFile
          val logLines:Seq[String] = Source.fromFile(logFile).getLines.toSeq.filter(x=>x.nonEmpty)
          if (version.isEmpty) {
            getLatestRepositoryEntry(logLines) match {
              case Some(entry) => Right(new File(targetFolder, SNAPSHOT_FOLDER_NAME+entry.version+File.separator))
              case None => Left()
            }
          }
          else {
            val entries = getRepositoryEntries(logLines)
            val set = entries.foldLeft(Set[String]()){case (set, entry) => set + entry.version}
            if (set.contains(version)) Right(new File(targetFolder, SNAPSHOT_FOLDER_NAME+version+File.separator)) else Left()
          }
        }
        branchFolder match {
          case Right(folder) => {
            FileUtils.copyDirectory(folder, thisFolder)
            None
          }

          case Left(_) => Some(VersionNotFoundException(s"version $version not found in branch $branchName"))
        }
      }
      case None => Some(BranchNotFoundException(s"branch $branchName does not exist"))
    }
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
        LogFileWriter.writeHeadFile(HEAD_FILE, parentVersion)
        None
      }
    }

  }

 private def createBranch(branchName: String, location: String): Option[GenError] = {
    val baseLogPath =  CURRENT_RUNNING_PATH + "/" + BASE_LOG
    val baseLogFile = new File(baseLogPath)
    if (!baseLogFile.exists())
      baseLogFile.createNewFile()
    val lines = Source.fromFile(baseLogFile).getLines().toSeq.filter(line => line.nonEmpty)
    LogFileReader.getBranchPath(lines, branchName) match {
      case Some(path) =>  Some(ExistingBranchException(s"Branch $branchName already exists"))
      case None => { LogFileWriter.updateBaseLog(baseLogPath, RepositoryInfo(branchName, location))
        new File(location, SNAPSHOT_FOLDER_NAME).mkdir()
        None
      }
    }
  }

  private def getSnapShotPath(version: String) = new File(CURRENT_RUNNING_PATH,SNAPSHOT_FOLDER_NAME + version).getAbsolutePath
}


