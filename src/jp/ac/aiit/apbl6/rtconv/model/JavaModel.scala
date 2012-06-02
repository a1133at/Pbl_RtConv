package jp.ac.aiit.apbl6.rtconv.model

import reflect.BeanProperty

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 22:56
 * To change this template use File | Settings | File Templates.
 */

case class JavaModel(@BeanProperty val packageName: String,
                     @BeanProperty val imports: Array[ImportModel],
                     @BeanProperty var body: JavaBodyModel) {
}
