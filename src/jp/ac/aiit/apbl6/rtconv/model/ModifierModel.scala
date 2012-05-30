package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:09
 * To change this template use File | Settings | File Templates.
 */
object ModifierModel {
  val ABSTRACT = ModifierModel("ABSTRACT")
  val PUBLIC = ModifierModel("PUBLIC")
  val PROTECTED = ModifierModel("PROTECTED")
  val PRIVATE = ModifierModel("PRIVATE")
  val STATIC = ModifierModel("STATIC")
}

class ModifierModel private (val name: String) {
}
