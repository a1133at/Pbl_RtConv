package jp.ac.aiit.apbl6.rtconv.model

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/24
 * Time: 22:56
 * To change this template use File | Settings | File Templates.
 */

case class JavaModel(var packageName: String, var imports: List[ImportModel], var body: JavaBodyModel) {
}
