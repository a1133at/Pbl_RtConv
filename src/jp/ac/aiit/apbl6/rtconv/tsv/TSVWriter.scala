package jp.ac.aiit.apbl6.rtconv.tsv

import jp.ac.aiit.apbl6.rtconv.model
import model.{InterfaceModel, ClassModel, JavaModel}

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/29
 * Time: 18:35
 * To change this template use File | Settings | File Templates.
 */

object TSVWriter {
  def write(models: Array[JavaModel]): Unit = {
    val packageMap = getPackageMap(models.toList)

    for (model <- models if model.body.isInstanceOf[ClassModel]){

    }
  }

  /**
   * create String List for Body of tsv
   * @param models
   * @return
   */
  def getBody(models: List[JavaModel], packMap: Map[String, Int]) :List[String] = {
    val f = List.empty[String]
    for (model <- models){
      model match {
        case c:ClassModel =>
        case i:InterfaceModel =>
      }
    }
  }

  def addClass(list: List[String], model: ClassModel) = {
    val classFromat = "{CLASS}\t{ID}\t{NAME}\t{VISIBILLITY}\t0\t0\t0\t0\t0\t\t{PACID}";
    list += classFromat.format(model

    )

  }


  /**
   * Create Map for Package Part of tsv
   * @param models
   * @return
   */
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
  }
}
