package cn.changhong.reflect.util


import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>rus}
/**
 * Created by yangguo on 15-3-10.
 *
 *
 * Unfortunately, in its current state released in Scala 2.10.0,
 * reflection is not thread safe. There’s a JIRA issue SI-6240(https://issues.scala-lang.org/browse/SI-6240),
 * which can be used to track our progress and to look up technical details,
 * and here’s a concise summary of the state of the art.
 * NEW Thread safety issues have been fixed in Scala 2.11.0-RC1,
 * but we are going to keep this document available for now,
 * since the problem still remains in the Scala 2.10.x series,
 * and we currently don't have concrete plans on when the fix is going to be backported.
 */
object NewInstance1 {
  implicit val mirror=rus.runtimeMirror(getClass.getClassLoader)
  def apply[T](values:Map[String,Any])(implicit t:TypeTag[T],mirror:Mirror):T={
    val clazzMirror=mirror.reflectClass(t.tpe.typeSymbol.asClass)
    val constructor=clazzMirror.reflectConstructor(t.tpe.declaration(rus.nme.CONSTRUCTOR).asMethod)
    val args=createConstructorArgs(values,constructor.symbol)
    constructor(args:_*).asInstanceOf[T]
  }
  def createConstructorArgs(values:Map[String,Any],methodSymbol:MethodSymbol):Seq[Any]={
    methodSymbol.paramss.flatMap(pp=>pp).map{p=>
      val value = values.get(p.name.toString)
      println(p.name.toString)
      if (p.typeSignature <:< typeOf[Option[_]]) value
      else {
        if (!value.isEmpty) value.get
        else {
          try{
            defaultVal(p)
          }catch{
            case ex:Throwable=>throw new IllegalArgumentException(s"TypeSafe Error!${methodSymbol.name} ,Argument Name[${p.name.toString}] Is Null!")
          }
        }
      }
    }
  }
  private[this] def defaultVal(symbol:Symbol)={
    symbol.typeSignature match{
      case t if t <:< typeOf[Int]||t<:<typeOf[Double]||t<:<typeOf[Long]||t<:<typeOf[Float]||t<:<typeOf[Char]=>0
      case t if t <:< typeOf[String]=>""
      case t if t <:< typeOf[Option[_]]=>None
      case t=> throw new IllegalArgumentException("Type Safe Error!")
    }
  }
}
case class Bean1(username:String,pass:String,age:Option[Int])
class UserBean{
  def p="h"
}
object Test{
  import NewInstance1._
  def main(args: Array[String]): Unit = {
    val args:Map[String,Any]=Map("username"->"username","pass"->"pass")
    val bean=NewInstance1[Bean1](args)
    println(bean.username+"sadsa"+bean.age.getOrElse(-1))
    val bean1=NewInstance1[UserBean](args)
    println(bean1.p)
  }

}