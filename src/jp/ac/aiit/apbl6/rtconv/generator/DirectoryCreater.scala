package jp.ac.aiit.apbl6.rtconv.generator

import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: ryu
 * Date: 12/06/06
 * Time: 23:19
 * To change this template use File | Settings | File Templates.
 */

class DirectoryCreater {
  def createDirectory(packageName: String): String = {
    var parent  =  ""
    val dirs = "output"::(packageName split Array('.')).toList
    for (dir <- dirs) {
      parent = parent + "." + File.separator + dir
      val newDir = new File(parent)
      if (!newDir.exists){
        newDir.mkdir()
      }
    }
    return parent
  }

}
