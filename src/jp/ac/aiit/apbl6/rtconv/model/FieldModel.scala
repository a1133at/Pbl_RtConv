package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:17
 * To change this template use File | Settings | File Templates.
 */

case class FieldModel(@BeanProperty val name: String,
                      @BeanProperty val isStatic: Boolean,
                      @BeanProperty val modifiers: Array[ModifierModel],
                      @BeanProperty val type_m: TypeModel)  extends IMemberModel {
}
