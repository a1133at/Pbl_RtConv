package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:23
 * To change this template use File | Settings | File Templates.
 */

trait IMemberModel {
  @BeanProperty val type_m: String
  @BeanProperty val modifiers: Array[ModifierModel]
  @BeanProperty val isStatic: Boolean
  @BeanProperty val name: String

  def isMethod = {
    this.isInstanceOf[MethodModel]
  }
}
