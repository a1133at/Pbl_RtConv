package jp.ac.aiit.apbl6.rtconv.generator

import java.io.PrintWriter
import java.io.File
import org.apache.velocity._
import java.lang.reflect.Modifier
import reflect.Field
import jp.ac.aiit.apbl6.rtconv.model._

/**
 * Created with IntelliJ IDEA.
 * User: ryu
 * Date: 12/05/28
 * Time: 1:34
 * To change this template use File | Settings | File Templates.
 */

object GeneratorMain {
  def main(args: Array[String]): Unit = {
    println("program start")

    // making test data
//    val import1 = new ImportModel("java.io.PrintWriter1")
//    val import2 = new ImportModel("java.io.PrintWriter2")
    val import1 = new ImportModel(" java.io.PrintWriter1")
    val import2 = new ImportModel("java.io.PrintWriter2")

    val javaModel = new JavaModel(
      "jp.co.hogehuga.implementation",
      Array(import1, import2),
      new InterfaceModel("testInterface", Array(new FieldModel("testField", false, Array(ModifierModel.PUBLIC), new TypeModel("int"))))
    )

    // use Velocity
    val velWrapper = new VelocityWrapper(File.separator + "template" + File.separator + "templateJava.vm")
    velWrapper.put("javaModel", javaModel)
    val result = velWrapper.merge()
    System.out.println("expand test data")
    val pw	= new PrintWriter("output.xml", "UTF-8")
    pw.print(result)
    System.out.println("output the data")
    pw.close()

    println("program end")
  }

}
