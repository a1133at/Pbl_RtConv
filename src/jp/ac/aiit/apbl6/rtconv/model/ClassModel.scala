package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:15
 * To change this template use File | Settings | File Templates.
 */

case class ClassModel(val name: String,
                 val modifiers: Array[ModifierModel],
                 val interfaces: Array[InterfaceModel],
                 val extendsClass: Array[ClassModel],
                 val members: Array[IMemberModel]
                  ) extends JavaBodyModel{

}
