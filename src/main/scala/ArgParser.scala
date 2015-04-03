/**
 * Created by gmgilmore on 4/3/15.
 */



import java.io.File
import Errors.{FileCopyFailedException, GenError}

import Util.Util._
import org.apache.commons.io.FileUtils



object ArgParser extends App {

  val SNAPSHOT_FOLDER_NAME = ".myvcs/"

  val USAGE =s"""USAGE 'snapshot' followed by the path of the folder that you want the snapshot to be placed."""
  val LOG_FILENAME = "myvcs_log.txt"

  args match {
    case Array("snapshot", targetFolder) => copyAllFiles(CURRENT_RUNNING_PATH, targetFolder) match {
      case None =>
      case Some(err) => println(err.message)
    }
    case _ => println(USAGE)
  }


  def copyAllFiles(rootPath: String, targetDirName: String): Option[GenError] = {
    try {
      val folder = new File(rootPath)
      val target = new File(targetDirName, SNAPSHOT_FOLDER_NAME)
      FileUtils.copyDirectory(folder, target)
      None
    }
    catch {
      case e: java.io.IOException => Some(FileCopyFailedException( s"""Unable to copy contents of $rootPath to ${rootPath + targetDirName}"""))

    }

  }


}


