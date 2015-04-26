package Logger

/**
 * Created by gmgilmore on 4/16/15.
 */

import java.io.File

import Errors.{FolderNotFoundException, GenError}
import difflib._
import scala.collection.JavaConversions._
import scala.io.Source

object FileDiffer{



  def diffTwoFiles(originalFilePath: String, revisedFilePath: String):String = {
    val builder = new StringBuilder
    def makeDiffLines(isPlus:Boolean, lines:List[String]):Unit = {
      lines.foreach{case line => builder.append{s"""${if (isPlus) "+" else "-"}       $line """ + "\n"}}
    }
    val differences = DiffUtils.diff(LogFileReader.getLines(originalFilePath).toList, LogFileReader.getLines(revisedFilePath).toList)

    differences.getDeltas.foreach{case delta => {
      delta.getType match {
        case Delta.TYPE.CHANGE => {
          makeDiffLines(false, delta.getOriginal.getLines.toList)
          makeDiffLines(true, delta.getRevised.getLines.toList)
        }
        case Delta.TYPE.DELETE => makeDiffLines(false, delta.getOriginal.getLines.toList)
        case Delta.TYPE.INSERT =>makeDiffLines(true, delta.getRevised.getLines.toList)
      }
    }}
    builder.toString
  }
  
  def diffTwoDirectories(originalDirectoryPath:String, revisedDirectoryPath:String):String = {
    val originalDir = new File(originalDirectoryPath)
    require(originalDir.isDirectory)
    val revisedDir = new File(revisedDirectoryPath)
    require(revisedDir.isDirectory)
//    def fixPath (revisedDirectoryPath:String, revisionSubPath:String):String = {
//     I: ~/Test/.myvcs/0/garbage/test.txt >> O: ~/Test/.myvcs//test.txt
//    }
    println(s"originalPath at start: $originalDirectoryPath")
    val builder = new StringBuilder
    def diffTwoDirectoriesHelper(revisedRelativePath:String):Option[GenError] = {
      println(s"revisedRelativePath: $revisedRelativePath")
      val revisedCurrentPath = revisedDirectoryPath + revisedRelativePath
      if (!revisedCurrentPath.endsWith(".myvcs")) {
        println("relative path should not contain myvcs  " + revisedCurrentPath)
        new File(revisedCurrentPath).listFiles().foreach { case file => {
          if (file.isDirectory && !file.getPath.contains(".myvcs" + File.separator)) diffTwoDirectoriesHelper(revisedCurrentPath + file.getName + File.separator)

          else if (!file.getPath.endsWith(".myvcs")) {
            val originalFile = new File(originalDirectoryPath, revisedRelativePath + file.getName)
            println(s"originalFile: $originalFile")
            if (originalFile.exists()) {
              builder.append(s"FILE NAME: ${file.getName} \n ")
              builder.append(diffTwoFiles(originalFile.getAbsolutePath, file.getAbsolutePath))

            }
            else {
              builder.append(s"FILE ADDED: ${file.getAbsolutePath} \n")
              val lines = Source.fromFile(file).getLines.toSeq.map(line => line.trim).filter(line => line.nonEmpty)
              lines.foreach(line => builder.append(s"+       $line"))
            }
          }
        }

        }
        None
      } else None
    }
    diffTwoDirectoriesHelper(File.separator)
    builder.toString()
  }

//  res.getDeltas.foreach(delta => println(delta.getType +  " original" + delta.getOriginal + " revised:" + delta.getRevised ))
}
