package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:15
 * To change this template use File | Settings | File Templates.
 */

case class ClassModel(@BeanProperty name: String,
                      @BeanProperty val modifiers: Array[ModifierModel],
                      @BeanProperty val Interfaces: Array[InterfaceModel],
                      @BeanProperty val members: Array[IMemberModel]
                       ) extends JavaBodyModel{

}

