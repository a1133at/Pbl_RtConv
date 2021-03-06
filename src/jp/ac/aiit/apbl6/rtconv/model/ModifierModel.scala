package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:09
 * To change this template use File | Settings | File Templates.
 */
object ModifierModel {
  val ABSTRACT = new ModifierModel("abstract")
  val PUBLIC = new ModifierModel("public")
  val PROTECTED = new ModifierModel("protected")
  val PRIVATE = new ModifierModel("private")
  val STATIC = new ModifierModel("static")
}

class ModifierModel private (@BeanProperty val name: String) {
}
