package jp.ac.aiit.apbl6.rtconv.generator

import java.io.PrintWriter
import java.io.File
import org.apache.velocity._
import java.lang.reflect.Modifier
import reflect.Field
import jp.ac.aiit.apbl6.rtconv.model._
//import tools.nsc.io.Directory

/**
 * Created with IntelliJ IDEA.
 * User: ryu
 * Date: 12/05/28
 * Time: 1:34
 * To change this template use File | Settings | File Templates.
 */

object GeneratorMain {
  def write(models: List[JavaModel], outputDir: String): Unit = {
    // use Velocity
    val velWrapper = new VelocityWrapper(File.separator + "template" + File.separator + "templateJava.vm")
    val modelArray = models.toArray
    for (model <- modelArray) {
      // create output directory
      val creater = new DirectoryCreater()
      creater.createDirectory(outputDir, model.getPackageName())

      if (model.getBody() != None) {
        velWrapper.put("javaModel", model)
        val result = velWrapper.merge()
        val pw	= new PrintWriter(outputDir + File.separator + model.getPackageName().replace(".", File.separator) + File.separator + model.getBody().get.getName() + ".java")
        pw.print(result)
        pw.close()
      }
    }
    println("program end")
  }
}
