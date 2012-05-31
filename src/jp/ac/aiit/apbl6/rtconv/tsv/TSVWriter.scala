package jp.ac.aiit.apbl6.rtconv.tsv

import jp.ac.aiit.apbl6.rtconv.model
import model._

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/29
 * Time: 18:35
 * To change this template use File | Settings | File Templates.
 */

object TSVWriter {
  def write(models: Array[JavaModel]): Unit = {
    val strList = getStrList(models.toList)
  }

  private def getStrList(models: List[JavaModel]): List[String] = {
    val packageMap = getPackageMap(models)
    val packageStrList = packageMap.map(p => "PACKAGE\t%s\t%s".format(p._2,p._1)).toList
    val bodyStrList = getStrListBody(models, packageMap)
    packageStrList++bodyStrList
  }


  /**
   * create String List for Body of tsv
   * @param models
   * @return
   */
  private def getStrListBody(models: List[JavaModel], packMap: Map[String, Int]) :List[String] = {
    List[String]()++models.map(m => (m.body, packMap(m.packageName)))
                          .zipWithIndex.flatMap({case t:((ClassModel,Int), Int) => toStrListClass(t._1._1, t._2, t._1._2)
                                                 case t:((InterfaceModel, Int), Int) => toStrListInf(t._1._1, t._2, t._1._2)
                                                 case _ => List[String]()})
  }

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

  private def isAbstract(modifiers: List[ModifierModel]) : Boolean = modifiers.contains(ModifierModel.ABSTRACT)

  private def getVisibility(modifiers: List[ModifierModel]) : String = {
    modifiers match {
      case m if m.contains(ModifierModel.PUBLIC) => ModifierModel.PUBLIC.name
      case m if m.contains(ModifierModel.PRIVATE) => ModifierModel.PRIVATE.name
      case m if m.contains(ModifierModel.PROTECTED) => ModifierModel.PROTECTED.name
      case _ => "none"
    }
  }

  /**
   * Create Map for Package Part of tsv
   * @param models
   * @return
   */
  private def getPackageMap(models: List[JavaModel]):Map[String,Int] =
    Map()++models.map(model => model.packageName).distinct.zipWithIndex

  /*
  def getPackageMap(models: List[JavaModel]):Map[String,Int] = {
    val i = 0
    //Worker Function
    def updateMap(a: Array[JavaModel], m: Map[String, Int]): Map[String,Int] = {
      if(a.isEmpty){
        m
      } else {
        if (!m.contains(a(0).packageName)){
          i += 1
          updateMap(a - a(0), m + (a(0).packageName -> i))
        } else {
          updateMap(a - a(0), m)
        }
      }
    }
    updateMap(models,Map.empty[String,Int])
  }*/
}
