package jp.ac.aiit.apbl6.rtconv.tsv

import io.Source
import jp.ac.aiit.apbl6.rtconv.model._

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/06/01
 * Time: 11:11
 * To change this template use File | Settings | File Templates.
 */

object TSVReader {

  def getModelFromTSV(path: String): List[JavaModel] = {
    val lines = Source.fromFile(path).getLines().toList //line
    val idxLines = lines.map(l => l.split("\t")).zipWithIndex.map(l => (l._2, l._1)).toMap //(idx, line)

    val packageMap = getMap(lines, "PACKAGE", 1, 2).map(i => (i._1.toInt, i._2)) //(Id, Name)
    val extendMap = getMap(lines, "EXTEND", 1, 2).map(i => (i._1.toInt, i._2.toInt)) //(SubId, SuperId)
    val classMap = idxLines.withFilter(l => l._2(0) == "CLASS" || l._2(0) == "@CLASS" ).
      map(i => (i._2(1).toInt, getClassModel(idxLines, i._1, extendMap)))
    val interfaceMap = idxLines.withFilter(l => l._2(0) == "INTERFACE").
      map(l => (l._2(1).toInt, getInterfaceModel(idxLines, l._1)))
    val classPackSet = idxLines.withFilter(l => l._2(0) == "CLASS" || l._2(0) == "@CLASS" || l._2(0) == "INTERFACE").
      map(i => (i._2(1).toInt, i._2(10)))

    List()++
      classMap.map(c => JavaModel(packageMap(classPackSet(c._1.toInt).toInt),null,c._2))++
      interfaceMap.map(c => JavaModel(packageMap(classPackSet(c._1.toInt).toInt),null,c._2))
  }

  def getClassModel(idxLines: Map[Int, Array[String]], idx: Int, extendMap: Map[Int, Int]): ClassModel = {
    ClassModel(idxLines(idx)(2),
      getModifiers(idxLines(idx)).toArray,
      getInterfaces(idxLines, idxLines.filter(l => extendMap(idxLines(idx)(1).toInt) == l._2(1).toInt &&
        l._2(0) == "INTERFACE").keySet.toList).toArray,
      getClassModel(idxLines, idxLines.find(l => extendMap(idxLines(idx)(1).toInt) == l._2(1).toInt  &&
        l._2(0) != "INTERFACE").get._1, extendMap),
      getMembers(classLines(idxLines, idx)).toArray)
  }

  def getInterfaces(idxList:Map[Int, Array[String]], idxs: List[Int]): List[InterfaceModel] = {
    List()++idxs.map(i => getInterfaceModel(idxList, i)).toList
  }

  def getInterfaceModel(idxLines: Map[Int, Array[String]], id: Int): InterfaceModel = {
    val line = idxLines.find(p => p._2(1) == id).get
    InterfaceModel(line._2(2), getMembers(classLines(idxLines, line._1)).toArray)
  }

  private def getMembers(lines: Map[Int, Array[String]]): List[IMemberModel] = {
    List()++lines.map({
      case l if l._2(0) == "ATTRIBUTE" =>
        FieldModel(l._2(1), false,getModifiers(l._2).toArray, l._2(2))
      case l if l._2(0).indexOf("METHOD") != -1 =>
        MethodModel(l._2(1), false, getModifiers(l._2).toArray, l._2(2) ,getParameters(lines, l._1))}).toList
  }

  private def getParameters(lines: Map[Int, Array[String]], idx: Int): Map[String, String] = {
    Map()++((idx + 1) to (lines.find(l => l._1 > idx && l._2(0) != "PARAM").get._1 - 1)).
      map(i => (lines(i)(1), lines(i)(2)))
  }

  private def getModifiers(ary: Array[String]):List[ModifierModel] = {
    var modifiers: List[ModifierModel] = List()
    ary.foreach({case "public" => modifiers = ModifierModel.PUBLIC :: modifiers
                 case "private" => modifiers = ModifierModel.PRIVATE :: modifiers
                 case "protected" => modifiers = ModifierModel.PROTECTED :: modifiers
                 case "@CLASS" => modifiers = ModifierModel.ABSTRACT :: modifiers})
    modifiers
  }

  private def isJavaBodyLine(str: String) =
    str == "CLASS" || str == "@CLASS" || str == "INTERFACE"

  private def getMap(lines: List[String], name: String, idx1: Int, idx2: Int): Map[String, String] =
    lines.withFilter(l => l.indexOf(name) == 0).map(l => {val s = l.split("\t"); (s(idx1), s(idx2))}).toMap

  private def classLines(idxLines: Map[Int, Array[String]], idx:Int): Map[Int, Array[String]] =
    idxLines.slice(idx, idxLines.find(l => l._1 > idx && (l._2.size == 0 || isJavaBodyLine(l._2(0)))).get._1)
}
