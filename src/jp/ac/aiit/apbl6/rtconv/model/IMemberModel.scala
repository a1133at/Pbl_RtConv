package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:23
 * To change this template use File | Settings | File Templates.
 */

trait IMemberModel {
  var type_m: TypeModel
  var modifiers: List[ModifierModel]
  var isStatic: Boolean
  var name: String
}
