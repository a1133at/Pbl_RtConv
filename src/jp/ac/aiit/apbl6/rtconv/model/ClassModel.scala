package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:15
 * To change this template use File | Settings | File Templates.
 */

case class ClassModel(@BeanProperty val name: String,
                      @BeanProperty val modifiers: Array[ModifierModel],
                      @BeanProperty val interfaces: Array[InterfaceModel],
                      @BeanProperty val extendsClass: Option[ClassModel],
                      @BeanProperty val members: Array[IMemberModel]
                  ) extends JavaBodyModel{

    def isModifiers():    Boolean = if ((modifiers == null)  || (modifiers.length == 0)) false else true
    def isInterfaces():   Boolean = if ((interfaces == null) || (interfaces.length == 0)) false else true
    def isExtendsClass(): Boolean = if (extendsClass.isEmpty) false else true

}
