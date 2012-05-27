package jp.ac.aiit.apbl6.rtconv.parser

/**
 * Created with IntelliJ IDEA.
 * User: S.H
 * Date: 12/05/27
 * Time: 11:25
 * To change this template use File | Settings | File Templates.
 */
import scala.util.parsing.combinator._

class JavaCodeParser extends JavaTokenParsers  {
  /*
   * Javaコード全体の定義
   * <Java>::= <MyPackage> {<Import>} ( <Class> | <Interface> )
   */
  def Java: Parser[Any] = MyPackage~rep(Import)~( Class | Interface )
  /*
   * パッケージ定義
   * <MyPackage>::=package<PackageName>
   * <PackageName>::= <Identifier> | <PackageName> . <Identifier>
   */
  def MyPackage: Parser[Any] = "package"~PackageName~";"
  def PackageName: Parser[Any] = repsep(Identifier, ".")
  /*
   * Import文定義
   * <Import>::= import { <ImportIdentifier> }
   * <ImportIdentifier>::= [a-zA-Z\*]\w*
   */
  def Import: Parser[Any] = "import"~repsep(ImportIdentifier, ".")~";"
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
  def Interface: Parser[Any] = rep(InterfaceModifier)~"interface"~Identifier~rep(ExtendsInterfaces)~InterfaceBody
  def InterfaceModifier: Parser[Any] = "public" | "abstract"
  def ExtendsInterfaces: Parser[Any] = "extends"~repsep(InterfaceType, ",")
  def InterfaceType: Parser[Any] = TypeName
  def InterfaceBody: Parser[Any] = "{"~rep(InterfaceMemberDeclaration)~"}"
  def InterfaceMemberDeclaration: Parser[Any] = ConstantDeclaration | AbstractMethodDeclaration
  def ConstantDeclaration: Parser[Any] = rep(ConstantModifiers)~MyType~VariableDeclarator~";"
  def ConstantModifiers: Parser[Any] = "public" | "static" | "final"
  def AbstractMethodDeclaration: Parser[Any] = rep(AbstractMethodModifier)~ResultType~MethodDeclarator~";"
  def AbstractMethodModifier: Parser[Any] = "public" | "abstract"


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
  def Class: Parser[Any] = rep(ClassModifier)~"class"~Identifier~rep(MySuper)~rep(MyInterface)~ClassBody
  def ClassModifier: Parser[Any] = "public" | "abstract" | "final"
  def MySuper: Parser[Any] = "extends"~ClassType
  def ClassType: Parser[Any] = PackageName
  def MyInterface: Parser[Any] = "implements"~repsep(InterfaceTypeList, ",")
  def InterfaceTypeList: Parser[Any] = PackageName
  def ClassBody: Parser[Any] = "{"~rep(ClassBodyDeclaration)~"}"
  def ClassBodyDeclaration: Parser[Any] = ClassMemberDeclaration | StaticInitializer
  def StaticInitializer: Parser[Any] = "static"~MyBlock
  def ClassConstractor: Parser[Any] = rep(MethodModifier)~MethodDeclarator~MethodBody
  def ClassMemberDeclaration: Parser[Any] = FieldDeclaration | MethodDeclaration | ClassConstractor

  /*
   * フィールド定義
   * <FieldDeclaration> ::= {<FieldModifier>} <type> <VariableDeclarator> ;
   * <FieldModifier> ::= public | protected | private | static | final | transient | volatile
   * <VariableDeclarator> ::= <VariableDeclaratorId>
   * <VariableDeclaratorId> ::= <Identifier> | <VariableDeclaratorId> [ ]
   */
  def FieldDeclaration: Parser[Any] = rep(FieldModifier)~MyType~VariableDeclarator~";"
  def FieldModifier: Parser[Any] = "public" | "protected" | "private" | "static" | "final" |
    "transient" | "volatile"
  def VariableDeclarator: Parser[Any] = VariableDeclaratorId
  //  def VariableDeclaratorId: Parser[Any] = Identifier | VariableDeclaratorId~"["~"]"
  def VariableDeclaratorId: Parser[Any] = Identifier | Identifier~"["~"]"
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
  def MethodDeclaration: Parser[Any] = MethodHeader~MethodBody
  def MethodHeader: Parser[Any] = rep(MethodModifier)~ResultType~MethodDeclarator
  def ResultType: Parser[Any] = MyType | "void"
  def MethodModifier: Parser[Any] = "public" | "protected" | "private" | "abstract" | "final" | "static"
  "synchronized" | "native"
  def MethodDeclarator: Parser[Any] = Identifier~"("~FormalParameterList~")"
  def FormalParameterList: Parser[Any] = repsep(FormalParameter, ",")
  def FormalParameter: Parser[Any] = MyType~VariableDeclaratorId
  def MethodBody: Parser[Any] = MyBlock | ";"

  /*
   *　Tokens
   *  <TypeName>::= <PackageName>
   */
  def TypeName: Parser[Any] = PackageName
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
  def MyType: Parser[Any] = MyPrimitiveType | MyReferenceType
  def MyPrimitiveType: Parser[Any] = MyNumericType | "boolean"
  def MyNumericType: Parser[Any] = IntegralType | FloatingPointType
  def IntegralType: Parser[String] = "byte" | "short" | "int" | "long" | "char"
  def FloatingPointType: Parser[String] = "float" | "double"
  def MyReferenceType: Parser[Any] = ClassOrInterfaceType | MyArrayType
  def ClassOrInterfaceType: Parser[Any] = MyClassType | MyInterfaceType
  def MyClassType: Parser[Any] = TypeName
  def MyInterfaceType: Parser[Any] = TypeName
  def MyArrayType: Parser[Any] = TypeName~"["~"]"
  /*
   * メソッド本体、コメント部分
   * <MyBlock>::= { <BlockStatements> }
   * <BlockStatements>::= "" ToDo(12.5.17)：要実装
   */
  def Identifier: Parser[String] = ident
  def MyBlock: Parser[Any] = "{"~BlockStatements~"}"
  def BlockStatements: Parser[Any] = ""



}
