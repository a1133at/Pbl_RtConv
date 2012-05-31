package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 23:09
 * To change this template use File | Settings | File Templates.
 */
trait JavaBodyModel {
  val name: String

  def isMethod = {
    this.isInstanceOf[InterfaceModel]
  }
}
