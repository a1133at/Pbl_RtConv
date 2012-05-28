package jp.ac.aiit.apbl6.rtconv.model

import com.sun.xml.internal.fastinfoset.algorithm.BooleanEncodingAlgorithm

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/05/25
 * Time: 0:17
 * To change this template use File | Settings | File Templates.
 */

class FieldModel(val name: String,
                 val isStatic: Boolean,
                 val modifiers: Array[ModifierModel],
                 val type_m: TypeModel)  extends IMemberModel {
}
