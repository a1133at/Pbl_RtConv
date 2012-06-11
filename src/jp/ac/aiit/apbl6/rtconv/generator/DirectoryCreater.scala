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
  def createDirectory(outputDir: String, packageName: String): String = {
    val output = new File(outputDir)
    if (!output.exists){
      output.mkdir()
    }

    var parent  =  outputDir
    val dirs = (packageName split Array('.')).toList
    for (dir <- dirs) {
      parent = File.separator + parent + "." + File.separator + dir
      val newDir = new File(parent)
      if (!newDir.exists){
        newDir.mkdir()
      }
    }
    return parent
  }

}
