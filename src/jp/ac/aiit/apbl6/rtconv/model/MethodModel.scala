package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:25
 * To change this template use File | Settings | File Templates.
 */

case class MethodModel(val name: String,
                  val isStatic: Boolean,
                  val modifiers: Array[ModifierModel],
                  val type_m: TypeModel,
                  var parameters: Map[String, TypeModel])
  extends IMemberModel {
}
