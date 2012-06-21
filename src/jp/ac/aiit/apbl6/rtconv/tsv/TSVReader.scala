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
    val lines = Source.fromFile(path).getLines().toList
    val idxLines = Source.fromFile(path).getLines().zipWithIndex.map(i => (i._2, i._1.split("\t"))).toMap //line
    //val idxLines = zipLines.map(i => (i._2 -> i._1.split("\t"))).toMap
    //val idxLines = lines.map(_.split("\t")).sortBy(_)zipWithIndex.map(l => (l._2, l._1)).toMap //(idx, line)

    val packageMap = getMap(idxLines, "PACKAGE", 1, 2).map(i => (i._1.toInt, i._2))++Map(0 -> "")  //(Id, Name)
    val extendMap = getMap(idxLines, "EXTEND", 1, 2).map(i => (i._1.toInt, i._2.toInt))++Map(0 -> 0) //(SubId, SuperId)
    val classMap = idxLines.withFilter(l =>
        l._2(0) == "CLASS" || l._2(0) == "@CLASS" ).
      map(i =>
        (i._2(1).toInt, getClassModel(idxLines, i._1, extendMap)))
    val interfaceMap = idxLines.withFilter(l => l._2(0) == "INTERFACE").
      map(l => (l._2(1).toInt, getInterfaceModel(idxLines, l._1)))
    val classPackSet = idxLines.withFilter(l => l._2(0) == "CLASS" || l._2(0) == "@CLASS" || l._2(0) == "INTERFACE").
      map(i => (i._2(1).toInt, if(i._2.length >= 11) i._2(10) else "0")
    )

    var result = List()++
      classMap.map(c => JavaModel(packageMap(classPackSet(c._1.toInt).toInt - 1),null,c._2))++
      interfaceMap.map(c => JavaModel(packageMap(classPackSet(c._1.toInt).toInt - 1),null,c._2))
    result
  }

  def getClassModel(idxLines: Map[Int, Array[String]], idx: Int, extendMap: Map[Int, Int]): Option[ClassModel] = {
    if (idxLines.isEmpty || idx == -1 || extendMap.isEmpty) return None
    Some(ClassModel(idxLines(idx)(2),
      getModifiers(idxLines(idx)).toArray,
      getInterfaces(idxLines, idxLines.filter(l => extendMap.contains(idxLines(idx)(1).toInt) &&
        l._2.length > 1 && l._2(1).forall{ _.isDigit }  &&
        extendMap(idxLines(idx)(1).toInt) == l._2(1).toInt &&
        l._2(0) == "INTERFACE").keySet.toList).toArray,
      getClassModel(idxLines, idxLines.find(l => extendMap.contains(idxLines(idx)(1).toInt) &&
        l._2.length > 1 && l._2(1).forall{ _.isDigit } &&
        extendMap(idxLines(idx)(1).toInt) == l._2(1).toInt  &&
        l._2(0) != "INTERFACE").getOrElse((-1, -1))._1, extendMap),
      getMembers(classLines(idxLines, idx)).toArray))
  }

  def getInterfaces(idxList:Map[Int, Array[String]], idxs: List[Int]): List[InterfaceModel] = {
    var list = List[InterfaceModel]()
    List()++idxs.map(i => getInterfaceModel(idxList, i)).collect({ case Some(j) => j }).toList
  }

  def getInterfaceModel(idxLines: Map[Int, Array[String]], id: Int): Option[InterfaceModel] = {
    val line = idxLines.filter(_._2.length > 2).find(p => p._2(1) == id).getOrElse(null)
    if (line == null) return None
    Some(InterfaceModel(line._2(2), getMembers(classLines(idxLines, line._1)).toArray))
  }

  private def getMembers(lines: Map[Int, Array[String]]): List[IMemberModel] = {
    List()++lines.collect({
      case l if l._2(0) == "ATTRIBUTE" =>
        FieldModel(l._2(1), false,getModifiers(l._2).toArray, l._2(2))
      case l if l._2(0).indexOf("METHOD") != -1 =>
        MethodModel(l._2(1), false, getModifiers(l._2).toArray,
          if(l._2(2) == "null") "" else l._2(2) ,getParameters(lines, l._1))})
  }

  private def getParameters(lines: Map[Int, Array[String]], idx: Int): Map[String, String] = {
    //val sLines = lines.toList.sortWith(_._1 < _._1)
    val sLines = lines
    if (((idx + 1) - sLines.find(l => l._1 > idx && l._2(0) != "PARAM").getOrElse((-1, -1))._1 - 1) >= 0)
      return Map()
    Map()++sLines.filter(a => (idx + 1) <= a._1 && (sLines.find(l => l._1 > idx && l._2(0) != "PARAM").get._1 - 1) >= a._1).
      map(i => (i._2(1), i._2(2)))
  }

  private def getModifiers(ary: Array[String]):List[ModifierModel] = {
    var modifiers: List[ModifierModel] = List()
    ary.foreach({case "public" => modifiers = ModifierModel.PUBLIC :: modifiers
                 case "private" => modifiers = ModifierModel.PRIVATE :: modifiers
                 case "protected" => modifiers = ModifierModel.PROTECTED :: modifiers
                 case "@CLASS" => modifiers = ModifierModel.ABSTRACT :: modifiers
                 case _ =>
    })
    modifiers
  }

  private def isJavaBodyLine(str: String) =
    str == "CLASS" || str == "@CLASS" || str == "INTERFACE"

  private def getMap(lines: Map[Int,Array[String]], name: String, idx1: Int, idx2: Int): Map[String, String] =
    lines.withFilter(l => l._2(0) == name).map(l =>
      (l._1.toString, l._2(idx2))).toMap

  private def classLines(idxLines: Map[Int, Array[String]], idx:Int): Map[Int, Array[String]] = {
    var result :Map[Int,Array[String]] = Map()
    val list = idxLines.toArray.sortBy(_._1)
    list.slice(idx, list.find(l => l._1 > idx && (l._2.size == 0 || isJavaBodyLine(l._2(0)))).getOrElse((idx+1,null))._1).
      foreach(i => result = result+(i._1 ->i._2))
    result
  }
}
