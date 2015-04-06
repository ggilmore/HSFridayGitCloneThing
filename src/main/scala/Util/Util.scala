package Util

import java.util.Date

/**
 * Created by gmgilmore on 4/3/15.
 */
object Util {

  val CURRENT_RUNNING_PATH: String = System.getProperty("user.dir")

  def getCurrentDate:String = new Date().toString

}
