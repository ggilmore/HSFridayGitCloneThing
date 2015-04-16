package Logger

/**
 * Created by gmgilmore on 4/16/15.
 */

import difflib._
import scala.collection.JavaConversions._

object FileDiffer extends App{



  def getDiffs(original: String, revised: String) = {
    DiffUtils.diff(LogFileReader.getLines(original).toList, LogFileReader.getLines(revised).toList)
  }

  val res = getDiffs("/Users/erisa/Downloads/filediff/bad.scala", "/Users/erisa/Downloads/filediff/good.scala")

  res.getDeltas.foreach(delta => println(delta.getType +  " original" + delta.getOriginal + " revised:" + delta.getRevised ))
}
