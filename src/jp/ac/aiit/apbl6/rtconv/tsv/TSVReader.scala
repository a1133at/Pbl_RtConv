package jp.ac.aiit.apbl6.rtconv.tsv

import io.Source
import jp.ac.aiit.apbl6.rtconv.model.{ModifierModel, IMemberModel, InterfaceModel, ClassModel}

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/06/01
 * Time: 11:11
 * To change this template use File | Settings | File Templates.
 */

object TSVReader {

  def getModelFromTSV(path: String) = {
    val lines = Source.fromFile(path).getLines().toList //line
    val idxLines = lines.map(l => l.split("\t")).zipWithIndex.map(l => (l._2, l._1)).toMap //(idx, line)
    val classLines = (idx:Int) => idxLines.slice(idx, idxLines.find(l => l._1 > idx && (l._2.size == 0 || isJavaBodyLine(l._2(0)))).get._1)

    val packageMap = getMap(lines, "PACKAGE", 1, 2).map(i => (i._1.toInt, i._2)) //(Id, Name)
    val extendMap = getMap(lines, "EXTEND", 1, 2).map(i => (i._1.toInt, i._2.toInt)) //(SubId, SuperId)
    val classMap = idxLines.withFilter(l => l._2(0) == "CLASS" || l._2(0) == "@CLASS" ).
          map(l => (l._2(1).toInt, getClassModel(l._1)))
    val interfaceMap = idxLines.withFilter(l => l._2(0) == "INTERFACE").map(l => (l._2(1).toInt, getClassModel(l._1)))

    def getClassModel(idx: Int): ClassModel = {
      ClassModel(idxLines(idx)(2),
        getModifiers(idxLines(idx)),
        getInterfaces(idxLines.filter(l => extendMap(idxLines(idx)(1)) == l._2(1) && l._2(0) == "INTERFACE").keys.toList),
        getClass(idxLines.find(l => extendMap(idxLines(idx)(1)) == l._2(1)  && l._2(0) != "INTERFACE").get._1),
        getMembers(classLines(idx)))
    }

    def getInterfaces(idxs: List[Int]): List[InterfaceModel] = {
      List()++idxs.map(i => getInterfaceModel(i)).toList
    }

    def getInterfaceMap(idx: Int): Map[Int, InterfaceModel] =
      InterfaceModel(idxLines(idx)(2), getMembers())

    //def getInterfaceModel(): InterfaceModel = {　　　　　
    //  InterfaceModel()
    //}
  }

  //private def getMembers(List[])

  private def getModifiers(ary: Array[String]):List[ModifierModel] = {
    var modifiers = List[ModifierModel]
    ary.foreach({case "public" => modifiers += ModifierModel.PUBLIC
                 case "private" => modifiers += ModifierModel.PRIVATE
                 case "protected" => modifiers += ModifierModel.PROTECTED
                 case "@CLASS" => modifiers += ModifierModel.ABSTRACT })
    modifiers
  }

  private def isJavaBodyLine(str: String) =
    str == "CLASS" || str == "@CLASS" || str == "INTERFACE"

  private def getMap(lines: List[String], name: String, idx1: Int, idx2: Int): Map[String, String] =
    lines.withFilter(l => l.indexOf(name) == 0).map(l => {val s = l.split("\t"); (s(idx1), s(idx2))}).toMap
}
