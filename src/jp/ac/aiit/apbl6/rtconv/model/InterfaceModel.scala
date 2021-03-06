package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:27
 * To change this template use File | Settings | File Templates.
 */

case class InterfaceModel(@BeanProperty val name: String,
                          @BeanProperty val members: Array[IMemberModel]) extends JavaBodyModel {
}
