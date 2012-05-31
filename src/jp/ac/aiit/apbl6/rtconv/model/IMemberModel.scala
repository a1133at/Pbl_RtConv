package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:23
 * To change this template use File | Settings | File Templates.
 */

trait IMemberModel {
  val type_m: TypeModel
  val modifiers: Array[ModifierModel]
  val isStatic: Boolean
  val name: String

  def isMethod = {
    this.isInstanceOf[MethodModel]
  }
}
