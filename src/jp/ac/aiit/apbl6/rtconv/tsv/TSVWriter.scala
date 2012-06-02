package jp.ac.aiit.apbl6.rtconv.tsv

import jp.ac.aiit.apbl6.rtconv.model
import model._
import java.io.{FileOutputStream, OutputStreamWriter}

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/29
 * Time: 18:35
 * To change this template use File | Settings | File Templates.
 */

object TSVWriter {


  def write(models: Array[JavaModel], filePath: String): Unit = {
    val writer = new OutputStreamWriter(new FileOutputStream(filePath, false),"UTF-8")
    getStrList(models.toList).foreach(line => writer.write(line + "\r\n"))
    writer.close()
  }

  private def getStrList(models: List[JavaModel]): List[String] = {
    //(PackageName,PackageId)
    val packageMap = getPackageMap(models)
    //(JavaBodyModel,PackageId,BodyId)
    val bodyIdMap = models.map(m => (m.body, packageMap(m.packageName))).zipWithIndex.map(t => (t._1._1, t._1._2, t._2))

    val packageStrList = packageMap.map(p => "PACKAGE\t%s\t%s".format(p._2,p._1)).toList
    val bodyStrList = getStrListBody(bodyIdMap)
    var relationStrList = getStrListRelation(bodyIdMap)
    packageStrList++bodyStrList++relationStrList
  }

  /**
   * create String List for Body of tsv
   */
  private def getStrListBody(bodyIdMap: List[(JavaBodyModel, Int, Int)]): List[String] = {
    List[String]()++bodyIdMap.flatMap({case t:(ClassModel,Int, Int) => toStrListClass(t._1, t._3, t._2)
                                       case t:(InterfaceModel, Int, Int) => toStrListInf(t._1, t._3, t._2)
                                       case _ => List[String]()})
  }
 // private def getBodyIdMap(models: List[JavaModel], packMap: Map[String, Int]): List[(JavaBodyModel, Int, Int)] = {null}


  private def toStrListClass(m: ClassModel, cId: Int, pId :Int): List[String] = {
    val classFormat = "%s\t%s\t%s\t%s\t0\t0\t0\t0\t0\t\t%s"
    val classType = if (isAbstract(m.modifiers.toList))  "@CLASS" else "CLASS"
    List(classFormat.format(classType, cId,m.name,getVisibility(m.modifiers.toList),pId))++
                            m.members.flatMap({case mem: FieldModel => toStrListField(mem)
                                               case mem: MethodModel => toStrListMethod(mem)
                                               case _ => List[String]()})
  }

  private def toStrListInf(m: InterfaceModel, cId: Int, pId :Int): List[String] = {
    val ifFormat = "INTERFACE\t%s\t%s\tPUBLIC\t0\t0\t0\t0\t0\t\t%s"
    List(ifFormat.format(cId, m.name, pId))++m.members.flatMap({case mem: MethodModel => toStrListMethod(mem)
                                                                case _ => List[String]()})
  }

  private def toStrListField(model :FieldModel): List[String] =
    List("ATTRIBUTE\t%s\t%s\t%s".format(model.name, model.type_m.name, getVisibility(model.modifiers.toList)))

  private def toStrListMethod(model :MethodModel): List[String] = {
    val methodType = if (isAbstract(model.modifiers.toList)) "@METHOD" else "METHOD"
    List("%s\t%s\t%s\t%s".format(methodType, model.name, model.type_m.name, getVisibility(model.modifiers.toList)))++
      model.parameters.map(p => "PARAMETER\t%s\t%s".format(p._1,p._2.name))
  }

  private def isAbstract(modifiers: List[ModifierModel]): Boolean = modifiers.contains(ModifierModel.ABSTRACT)

  private def getVisibility(modifiers: List[ModifierModel]): String = {
    modifiers match {
      case m if m.contains(ModifierModel.PUBLIC) => ModifierModel.PUBLIC.name
      case m if m.contains(ModifierModel.PRIVATE) => ModifierModel.PRIVATE.name
      case m if m.contains(ModifierModel.PROTECTED) => ModifierModel.PROTECTED.name
      case _ => "none"
    }
  }

  private def getStrListRelation(bodyIdMap: List[(JavaBodyModel, Int, Int)]): List[String] = {
    List()
  }

  /**
   * Create Map for Package Part of tsv
   * @param models
   * @return
   */
  private def getPackageMap(models: List[JavaModel]): Map[String,Int] =
    Map()++models.map(model => model.packageName).distinct.zipWithIndex.map(m => (m._1, m._2 + 1))
}
