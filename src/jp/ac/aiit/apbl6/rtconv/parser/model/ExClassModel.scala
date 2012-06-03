package jp.ac.aiit.apbl6.rtconv.parser.model

/**
 * Created with IntelliJ IDEA.
 * User: HEYA
 * Date: 12/05/29
 * Time: 14:58
 * To change this template use File | Settings | File Templates.
 */

import jp.ac.aiit.apbl6.rtconv.model._

class ExClassModel(name: String,
                   modifiers: Array[ModifierModel],
                   interfaces: Array[InterfaceModel],
                   extendClass: ClassModel,
                   members: Array[IMemberModel],
                   val exClassName : List[String],
                   val exInterfaces: List[String])
  extends ClassModel(name,modifiers,interfaces,extendClass,members){

}
