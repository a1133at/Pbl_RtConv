package jp.ac.aiit.apbl6.rtconv.generator

import java.io.PrintWriter
import java.io.File
import org.apache.velocity._
import java.lang.reflect.Modifier
import reflect.Field
import jp.ac.aiit.apbl6.rtconv.model._
import tools.nsc.io.Directory

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
    for (model <- models) {
      // create output directory
      val creater = new DirectoryCreater()
      creater.createDirectory(outputDir,model.getPackageName())

      velWrapper.put("javaModel", model)
      val result = velWrapper.merge()

      val pw	= new PrintWriter(model.getPackageName() + File.separator + model.getBody().getName() + ".java", "UTF-8")
      pw.print(result)
      pw.close()
    }
  }
}
