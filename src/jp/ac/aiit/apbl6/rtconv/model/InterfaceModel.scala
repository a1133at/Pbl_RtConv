package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:27
 * To change this template use File | Settings | File Templates.
 */

case class InterfaceModel(val name: String, val members: Array[IMemberModel]) extends JavaBodyModel {
}
