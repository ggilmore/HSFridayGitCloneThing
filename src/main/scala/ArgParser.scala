/**
 * Created by gmgilmore on 4/3/15.
 */



import java.io.{FileFilter, File}
import java.util
import Errors.{BranchNotFoundException, GenError, ExistingBranchException, FileCopyFailedException}
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

  args match {
    case Array("create", name, location) => createBranch(name, location) match {
      case Some(err) => println(err.message)
      case None =>
    }
    case Array("snapshot", branchName) => commit(branchName=branchName) match {
      case Some(err) => println(err.message)
      case None =>
    }
    case Array("snapshot", "-m", message, branchName) => commit(message, branchName) match {
      case Some(err) => println(err.message)
      case None =>
    }
    case Array("checkout", version, branchName) => ???
    case _ => println(USAGE)
  }


  def commit(message:String = "", branchName:String): Option[GenError] = {
    LogFileReader.getBranchPath(Source.fromFile(new File(CURRENT_RUNNING_PATH, BASE_LOG)).getLines().toSeq.filter(line => line.nonEmpty), branchName) match {
      case Some(targetFolder) => {
        val logFile = new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME)
        if (!logFile.exists) logFile.createNewFile
        val logLines:Seq[String] = Source.fromFile(logFile).getLines.toSeq.filter(x=>x.nonEmpty)
        println(s"logLines $logLines")
        getLatestRepositoryEntry(logLines) match {
          case Some(entry) => createNewSnapShot((entry.version.toInt +1).toString, "",
            CURRENT_RUNNING_PATH, targetFolder, new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
          case None => createNewSnapShot(0.toString, "",
            CURRENT_RUNNING_PATH, targetFolder, new File(targetFolder, SNAPSHOT_FOLDER_NAME+LOG_FILENAME).getAbsolutePath)
            None
        }
      }
      case None => Some(BranchNotFoundException(s"branch $branchName does not exist"))
    }
  }

  private def copyAllFiles(rootPath: String, targetDirName: String, snapVersion:String): Option[GenError] = {
    try {
      val folder = new File(rootPath)
      val target = new File(targetDirName, SNAPSHOT_FOLDER_NAME + snapVersion + File.separator)
      println(target.mkdir)
      val filter = new FileFilter {
        override def accept(pathname: File): Boolean = !pathname.getName().equalsIgnoreCase(BASE_LOG)
      }
      FileUtils.copyDirectory(folder, target, filter)
      None
    }
    catch {
      case e: java.io.IOException => Some(FileCopyFailedException( s"""Unable to copy contents of $rootPath to ${rootPath + targetDirName}"""))

    }

  }

  def createNewSnapShot(newVersion:String, message:String ="", rootPath:String, targetDirName:String, logPath:String):Option[GenError] = {
    writeToRepositoryLog(logPath, Entry(newVersion, message))
    copyAllFiles(rootPath, targetDirName, newVersion)
  }

  def createBranch(branchName: String, location: String): Option[GenError] = {
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




}


