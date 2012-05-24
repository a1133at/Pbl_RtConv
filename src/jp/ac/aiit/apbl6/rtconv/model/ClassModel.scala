package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:15
 * To change this template use File | Settings | File Templates.
 */

class ClassModel(var name: String,
                 var modifiers: List[ModifierModel],
                 var Interfaces: List[InterfaceModel],
                 var members: List[IMemberModel]
                  ) extends JavaBodyModel(name) {

}
