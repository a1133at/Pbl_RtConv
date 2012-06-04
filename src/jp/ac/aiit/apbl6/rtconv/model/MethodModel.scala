package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:25
 * To change this template use File | Settings | File Templates.
 */

case class MethodModel(@BeanProperty val name: String,
                       @BeanProperty val isStatic: Boolean,
                       @BeanProperty val modifiers: Array[ModifierModel],
                       @BeanProperty val type_m: TypeModel,
                       @BeanProperty var parameters: Map[String, String])
  extends IMemberModel {

  def getParametersAsArray(): Array[(String,String)] = parameters.toArray
}
