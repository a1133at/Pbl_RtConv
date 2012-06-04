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
    var javaModel = new JavaModel(
        "jp.co.hogehuga.implementation",
        Array(new ImportModel("java.io.PrintWriter1"), new ImportModel("java.io.PrintWriter2")),
        new InterfaceModel("IConstants",  Array(
          new FieldModel("iIndex",   false, Array(ModifierModel.PUBLIC,  ModifierModel.STATIC), new TypeModel("int")),
          new FieldModel("strField", false, Array(ModifierModel.PRIVATE, ModifierModel.STATIC), new TypeModel("String"))
        )
        )
      )

    if (true){
        javaModel = new JavaModel(
          "jp.co.hogehuga.implementation",
          Array(new ImportModel("java.io.PrintWriter1"), new ImportModel("java.io.PrintWriter2")),
            new ClassModel(  "CMetaData",
                              Array(ModifierModel.PUBLIC, ModifierModel.STATIC),
                              Array(
                                   new InterfaceModel("IConstants",
                                     Array(
                                         new FieldModel("iIndex",   false, Array(ModifierModel.PUBLIC,  ModifierModel.STATIC), new TypeModel("int")),
                                         new FieldModel("strField", false, Array(ModifierModel.PRIVATE, ModifierModel.STATIC), new TypeModel("String"))
                                     )
                                   )
                               ),
                              null,
                              Array(
                                   new FieldModel("iIndex",  false, Array(ModifierModel.PUBLIC,  ModifierModel.STATIC), new TypeModel("int")),
                                   new MethodModel("createObject", false, Array(ModifierModel.PUBLIC,  ModifierModel.STATIC), new TypeModel("Object"), Map("dbVariant"-> "double", "iVal" -> "int"))
                               )
          )
        )
    }
    // use Velocity
    val velWrapper = new VelocityWrapper(File.separator + "template" + File.separator + "templateJava.vm")
    velWrapper.put("javaModel", javaModel)
    val result = velWrapper.merge()
    System.out.println("expand test data")
    val pw	= new PrintWriter("output.txt", "UTF-8")
    pw.print(result)
    System.out.println("output the data")
    pw.close()

    println("program end")
  }

}
