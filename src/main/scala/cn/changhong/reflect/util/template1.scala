package cn.changhong.reflect.util


import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>rus}
/**
 * Created by yangguo on 15-3-10.
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