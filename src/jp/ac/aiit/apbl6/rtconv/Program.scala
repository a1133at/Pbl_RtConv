package jp.ac.aiit.apbl6.rtconv

import generator.GeneratorMain
import parser.JavaCodeParser
import tsv.{TSVWriter, TSVReader}

/**
 * Created with IntelliJ IDEA.
 * User: ATakahashi
 * Date: 12/06/07
 * Time: 14:18
 * To change this template use File | Settings | File Templates.
 */

object Program {
  def main(args: Array[String]) = {
    try{
    args(0) match {
      case "t2c" => GeneratorMain.write(TSVReader.getModelFromTSV(args(1)), args(2))

      case "c2t" => TSVWriter.write(JavaCodeParser.ParseJavaCode(Array(args(1))).toList, args(2))
    }
    } catch {
      case e: Exception => e.printStackTrace();
    }
  }
}
