package jp.ac.aiit.apbl6.rtconv.parser

/**
 * Created with IntelliJ IDEA.
 * User: S.H
 * Date: 12/05/27
 * Time: 11:25
 * To change this template use File | Settings | File Templates.
 */

import model.ExClassModel
import scala.util.parsing.combinator._
import jp.ac.aiit.apbl6.rtconv.model._
import io.Source
import java.io.{File, FileReader}

object JavaCodeParser extends JavaCodeParser {
  def ParseJavaCode(dirPath:String): Array[JavaModel] = {
    val fileList = getFileFromDir(dirPath)
    val list = fileList.map( p => delete( Source.fromFile(p).getLines().mkString("\n").toCharArray ).mkString )
    val res = list.map( p => parseAll(Java, p))
    val javaModels = List[JavaModel]() ++ list.map( p => parseAll(Java, p) .get)
    //クラス(_1)とインターフェース(_2)
    val clsinf: (List[JavaModel], List[JavaModel]) = javaModels.span( _.body.isInstanceOf[ExClassModel] )
    //継承あり(_1)となし(_2)
    val exCls: (List[JavaModel], List[JavaModel]) = clsinf._1.span( existExtendCls(_) )
    ( List() ++ exCls._1.map( getExtendInf( _, javaModels ) ) ++ clsinf._2 ++ exCls._2 ).toArray
  }

  def getFileFromDir(dir: String): List[String] = {
    var files = List[String]()
    //ディレクトリ(_1)とファイル(_2)
    val map = new File(dir).listFiles().toList.partition( _.isDirectory )
    files = files ++ map._2.filter( _.getPath.endsWith(".java") ).map( _.getPath )
    for (dir <- map._1){
      files = files ++ getFileFromDir(dir.getPath)
    }
    files
  }
  def delete(file: Array[Char]): Array[Char] = {
    val s = file.mkString
    file.take( file.indexOf('}') ).count( p => p == '{' ) match {
      case x if ( x >= 2 ) =>{
        val sp = file.splitAt( file.indexOf('}')+1 )
        delete(
          sp._1.take( sp._1.lastIndexWhere( p => p == '{') ) ++ sp._2
        )
      }
      case _ => { file }
    }
  }

  def existExtendCls( javaModel:JavaModel ):Boolean = {
    if( 0 != javaModel.body.asInstanceOf[ExClassModel].exClassName.size ||
      0 != javaModel.body.asInstanceOf[ExClassModel].exInterfaces.size){
      true
    }
    else{
      false
    }
  }

  def getExtendInf(exCls:JavaModel, javaList:List[JavaModel]): JavaModel = {
    val cls = exCls.body.asInstanceOf[ExClassModel]
    val ex = javaList.find( a =>  a.body.isDefined && a.body.get.name == cls.exClassName(0) )
    var list = List[JavaModel]()
    for ( name <- cls.exInterfaces ){
      val op: Option[JavaModel] = javaList.find( a =>  a.body.isDefined && a.body.get.name == name )
      if ( op != None ){
        op.get :: list
      }
    }
    new JavaModel(
      exCls.packageName,
      exCls.imports,
      Option(new ClassModel(
        cls.name,
        cls.modifiers,
        list.map( p => p.body.asInstanceOf[InterfaceModel] ).toArray,
        if(ex != None) Option(ex.get.body.asInstanceOf[ClassModel]) else None,
        cls.members
      ))
    )
  }

}

class JavaCodeParser extends JavaTokenParsers  {
  /*
   * Javaコード全体の定義
   * <Java>::= <MyPackage> {<Import>} ( <Class> | <Interface> )
   */
  def Java: Parser[JavaModel] = MyPackage~rep(Import)~( Class | Interface ) ^^ {
    case myPackage~importList~javaBody => {
      new JavaModel(myPackage, importList.toArray, Option(javaBody))
    }
  }
  /*
   * パッケージ定義
   * <MyPackage>::=package<PackageName>
   * <PackageName>::= <Identifier> | <PackageName> . <Identifier>
   */
  def MyPackage: Parser[String] = "package"~PackageName~";" ^^ {
    case "package"~packageName~";" => packageName
  }
  def PackageName: Parser[String] = repsep(Identifier, ".") ^^ (_.mkString("."))
  /*
   * Import文定義
   * <Import>::= import { <ImportIdentifier> }
   * <ImportIdentifier>::= [a-zA-Z\*]\w*
   */
  def Import: Parser[ImportModel] = "import"~repsep(ImportIdentifier, ".")~";" ^^ {
    case "import"~importIdentifierList~";" => new ImportModel( importIdentifierList.mkString(".") )
  }
  def ImportIdentifier: Parser[String] = """[a-zA-Z\*]\w*""".r
  /*
   * インターフェース定義
   *  <Interface> ::= {<InterfaceModifier>} interface <Identifier> <ExtendsInterfaces>? <InterfaceBody>
   *  <InterfaceModifier> ::= public | abstract
   *  <ExtendsInterfaces> ::= extends <InterfaceType> | <ExtendsInterfaces> , <InterfaceType>
   *  <InterfaceType> ::= <TypeName>
   *  <InterfaceBody> ::= "{" {<InterfaceMemberDeclaration>} "}"
   *  <InterfaceMemberDeclaration> ::= <ConstantDeclaration> | <AbstractMethodDeclaration>
   *  <ConstantDeclaration> ::= <constant modifiers> <type> <variable declarator> ;
   *  <ConstantModifiers> ::= public | static | final
   *  <AbstractMethodDeclaration>::= {<abstract method modifier>} <result type> <method declarator> ;
   *  <AbstractMethodModifier> ::= public | abstract
   */
  def Interface: Parser[InterfaceModel] =
    rep(InterfaceModifier)~"interface"~Identifier~rep(ExtendsInterfaces)~InterfaceBody ^^ {
      case interfaceModifier~"interface"~identifier~extendsInterfaces~interfaceBody => {
        new InterfaceModel( identifier, interfaceBody.toArray )
      }
    }
  def InterfaceModifier: Parser[Any] = "public" | "abstract"
  def ExtendsInterfaces: Parser[Any] = "extends"~repsep(InterfaceType, ",")
  def InterfaceType: Parser[Any] = TypeName

  def InterfaceBody: Parser[List[IMemberModel]] = "{"~rep(InterfaceMemberDeclaration)~"}" ^^ {
    case "{"~interfaceMemberDeclaration~"}" => interfaceMemberDeclaration
  }

  def InterfaceMemberDeclaration: Parser[IMemberModel] =(
    ConstantDeclaration ^^ ( x => x )
      | AbstractMethodDeclaration ^^ ( x => x ))

  def ConstantDeclaration: Parser[FieldModel] = rep(ConstantModifiers)~MyType~VariableDeclarator~";" ^^ {
    case constantModifiers~myType~variableDeclarator~";" => {
      new FieldModel(
        variableDeclarator,
        constantModifiers.exists( elm => (elm.name == "static") ),
        constantModifiers.toArray,
        myType.name
      )
    }
  }

  def ConstantModifiers: Parser[ModifierModel] = (
    "public"   ^^ ( x => ModifierModel.PUBLIC )
      | "static"   ^^ ( x => ModifierModel.STATIC )
      | "final"    ^^ ( x => null ) )

  def AbstractMethodDeclaration: Parser[ MethodModel ] =
    rep(AbstractMethodModifier)~ResultType~MethodDeclarator~";" ^^ {
      case abstractMethodModifier~resultType~methodDeclarator~";" => {
        new MethodModel(
          methodDeclarator._1,
          abstractMethodModifier.exists( e => e.name == "static" ),
          abstractMethodModifier.toArray,
          resultType.name,
          methodDeclarator._2.map( p => new Pair(p._1, p._2.name) ))
      }
    }

  def AbstractMethodModifier: Parser[ModifierModel] = (
    "public"    ^^ ( x => ModifierModel.PUBLIC )
      | "abstract"  ^^ ( x => ModifierModel.ABSTRACT ) )


  /*
   * クラス定義
   * <Class>::= {<ClassModifier>} class <Identifier> {<MySuper>} {<MyInterface>} <ClassBody>
   * <ClassModifier>::= public | abstract | final
   * <MySuper>::= extends <ClassType>
   * <ClassType> = <PackageName>
   * <MyInterface>::= implements { <InterfaceTypeList> , }
   * <InterfaceTypeList>::= <PackageName>
   * <ClassBody>::= { <ClassBodyDeclaration> }
   * <ClassBodyDeclaration>::= <ClassMemberDeclaration> | <StaticInitializer>
   */
  def Class: Parser[ClassModel] =
    rep(ClassModifier)~"class"~Identifier~rep(MySuper)~rep(MyInterface)~ClassBody ^^ {
      case classModifier~"class"~identifier~mySuper~myInterface~classBody => {
        new ExClassModel(
          identifier,
          classModifier.toArray,
          Array(),
          null,
          classBody.toArray,
          mySuper,
          if(0!=myInterface.size) myInterface(0).split(",").toList else List()
        )
      }
    }

  def ClassModifier: Parser[ModifierModel] = (
    "public"   ^^ ( x => ModifierModel.PUBLIC )
      | "abstract" ^^ ( x => ModifierModel.ABSTRACT )
      | "final"    ^^ ( x => null ) )

  def MySuper: Parser[String] = "extends"~ClassType ^^ {
    case "extends"~classType => classType
  }
  def ClassType: Parser[String] = PackageName

  def MyInterface: Parser[String] = "implements"~repsep(InterfaceTypeList, ",") ^^ {
    case "implements"~interfaceTypeList => interfaceTypeList.mkString(",")
  }

  def InterfaceTypeList: Parser[String] = PackageName

  def ClassBody: Parser[List[IMemberModel]] = "{"~rep(ClassBodyDeclaration)~"}" ^^ {
    case "{"~classBodyDeclaration~"}" => classBodyDeclaration
  }

  def ClassBodyDeclaration: Parser[IMemberModel] = ( ClassMemberDeclaration ^^ ( x => x )
    | StaticInitializer ^^ ( x => x ) )

  def StaticInitializer: Parser[MethodModel] = "static"~MyBlock ^^ {
    case "static"~myBlock => new MethodModel(
      "",
      true,
      ( Array():Array[ModifierModel] ),
      "",
      (Map():Map[String,String])
    )
  }

    def ClassMemberDeclaration: Parser[IMemberModel] =(
        MethodDeclaration ^^ ( x => x )
      | ClassConstractor ^^ ( x => x )
      | FieldDeclaration ^^ ( x => x ) )

  def ClassConstractor: Parser[MethodModel] = rep(MethodModifier)~MethodDeclarator ^^ {
    case methodModifier~methodDeclarator => {
      new MethodModel(
        methodDeclarator._1,
        methodModifier.exists( e => ( e.name == "static" ) ),
        methodModifier.toArray,
        "",
        methodDeclarator._2.map( p => new Pair(p._1, p._2.name) )
      )
    }
  }

  /*
   * フィールド定義
   * <FieldDeclaration> ::= {<FieldModifier>} <type> <VariableDeclarator> ;
   * <FieldModifier> ::= public | protected | private | static | final | transient | volatile
   * <VariableDeclarator> ::= <VariableDeclaratorId>
   * <VariableDeclaratorId> ::= <Identifier> | <VariableDeclaratorId> [ ]
   */
  def FieldDeclaration: Parser[FieldModel] = rep(FieldModifier)~MyType~VariableDeclarator~";" ^^ {
    case fieldModifier~myType~variableDeclarator~";" => {
      new FieldModel(
        variableDeclarator,
        fieldModifier.exists( elm => if (elm != null) elm.name == "static" else false ),
        fieldModifier.toArray,
        myType.name)
    }
  }

  def FieldModifier: Parser[ModifierModel] =(   "public"      ^^ (x => ModifierModel.PUBLIC )
    | "protected"   ^^ (x => ModifierModel.PROTECTED)
    | "private"     ^^ (x => ModifierModel.PRIVATE)
    | "static"      ^^ (x => ModifierModel.STATIC)
    | "final"       ^^ (x => null)
    | "transient"   ^^ (x => null)
    | "volatile"    ^^ (x => null) )

  def VariableDeclarator: Parser[String] = VariableDeclaratorId
  //  def VariableDeclaratorId: Parser[Any] = Identifier | VariableDeclaratorId~"["~"]"
  def VariableDeclaratorId: Parser[String] = (
        Identifier ^^ (x => x)
      | Identifier~"["~"]" ^^ {
      case identifier~"["~"]" => identifier + "[]"
    }
    )
  /*
  *　メソッド定義
  * <MethodDeclaration> ::= <MethodHeader> <MethodBody>
  * <MethodHeader> ::= {<MethodModifier>} <ResultType> <MethodDeclarator>
  * <ResultType> ::= <MyType> | void
  * <MethodModifier> ::= public | protected | private | static | abstract | final | synchronized | native
  * <MethodDeclarator> ::= <Identifier> ( <FormalParameterList>? )
  * <FormalParameterList> ::= <FormalParameter> | <FormalParameterList> , <FormalParameter>
  * <FormalParameter> ::= <MyType> <VariableDeclaratorId>
  * <method body> ::= <MyBlock> | ;
  */
  def MethodDeclaration: Parser[MethodModel] = MethodHeader ^^ {
    case methodHeader => {
      new MethodModel(
        methodHeader._3._1,
        methodHeader._1.exists( e => e.name == "static" ),
        methodHeader._1.toArray,
        methodHeader._2.name,
        methodHeader._3._2.map( p => new Pair(p._1, p._2.name) )
      )
    }
  }

  def MethodHeader: Parser[ Tuple3[ List[ModifierModel], TypeModel, Pair[ String, Map[String, TypeModel] ] ] ]
  = rep(MethodModifier)~ResultType~MethodDeclarator ^^ {
    case methodModifier~resultType~methodDeclarator => {
      new Tuple3( methodModifier, resultType, methodDeclarator )
    }
  }

  def ResultType: Parser[TypeModel] =(  MyType
    | "void" ^^ ( x => new TypeModel(x) ) )

  def MethodModifier: Parser[ModifierModel] =(
    "public"        ^^ ( x => ModifierModel.PUBLIC )
      | "protected"      ^^ ( x => ModifierModel.PROTECTED )
      | "private"      ^^ ( x => ModifierModel.PRIVATE )
      | "abstract"      ^^ ( x => ModifierModel.ABSTRACT )
      | "final"         ^^ ( x => null )
      | "static"        ^^ ( x => ModifierModel.STATIC )
      | "synchronized" ^^ ( x => null )
      | "native"         ^^ ( x => null ) )
  def MethodDeclarator: Parser[ Pair[ String, Map[String, TypeModel] ] ] = Identifier~"("~FormalParameterList~")" ^^ {
    case identifier~"("~formalParameterList~")" => {
      Pair(identifier, formalParameterList)
    }
  }
  def FormalParameterList: Parser[ Map[String, TypeModel] ] = repsep(FormalParameter, ",") ^^ ( x => x.toMap )
  def FormalParameter: Parser[Pair[String, TypeModel]] = MyType~VariableDeclaratorId ^^ {
    case myType~variableDeclaratorId => (variableDeclaratorId, myType)
  }
  def MethodBody: Parser[Any] = MyBlock | ";"

  /*
   *　Tokens
   *  <TypeName>::= <PackageName>
   */
  def TypeName: Parser[String] = PackageName
  /*
   * Types
   * <MyType> ::= <MyPrimitiveType> | <MyReferenceType>
   * <MyPrimitiveType> ::= <MyNumericType> | boolean
   * <MyNumericType> ::= <IntegralType> | <FloatingPointType>
   * <IntegralType> ::= byte | short | int | long | char
   * <FloatingPointType> ::= float | double
   * <MyReferenceType> ::= <class or interface type> | <array type>
   * <ClassOrInterfaceType> ::= <class type> | <interface type>
   * <MyClassType> ::= <type name>
   * <MyInterfaceType> ::= <type name>
   */
  def MyType: Parser[TypeModel] = (
    MyPrimitiveType ^^ ( x => new TypeModel(x) )
      | MyReferenceType ^^ ( x => new TypeModel(x) ) )

  def MyPrimitiveType: Parser[String] = MyNumericType | "boolean"
  def MyNumericType: Parser[String] = IntegralType | FloatingPointType
  def IntegralType: Parser[String] = "byte" | "short" | "int" | "long" | "char"
  def FloatingPointType: Parser[String] = "float" | "double"
  def MyReferenceType: Parser[String] = MyArrayType | ClassOrInterfaceType
  def ClassOrInterfaceType: Parser[String] = MyClassType | MyInterfaceType
  def MyClassType: Parser[String] = TypeName
  def MyInterfaceType: Parser[String] = TypeName
  def MyArrayType: Parser[String] = TypeName~"["~"]" ^^ {
    case typeName~"["~"]" => typeName + "[]"
  }
  /*
   * メソッド本体、コメント部分
   * <MyBlock>::= { <BlockStatements> }
   * <BlockStatements>::= "" ToDo(12.5.17)：要実装
   */
  def Identifier: Parser[String] = """[0-9a-zA-Z_><]*""".r
  def MyBlock: Parser[Any] = "{"~BlockStatements~"}"
  def BlockStatements: Parser[Any] = ""



}
