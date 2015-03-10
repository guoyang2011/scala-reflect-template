package cn.changhong.reflect.util

import scala.reflect.ClassTag
import scala.reflect.runtime.{currentMirror=>cm}
import scala.reflect.runtime.universe._
/**
 * Created by yangguo on 15-3-9.
 */
@deprecated("Use NewInstance1")
object NewInstance{
  def apply[A](args:Map[String,Any])(implicit t:ClassTag[A]):A={
    val claas=cm.classSymbol(t.runtimeClass)
    val module=claas.companionSymbol.asModule
    val im=cm.reflect(cm.reflectModule(module).instance)
    newInstance[A](im,"apply",args)
  }
  private[this] def newInstance[A](im:InstanceMirror,methodName:String,values:Map[String,Any]):A= {
    val at = newTermName(methodName)
    val ts = im.symbol.typeSignature
    val method = (ts.member(at)).asMethod
    val args=(for (pp <- method.paramss; p <- pp) yield p).map { p =>
      val value = values.get(p.name.toString)
      if (p.typeSignature <:< typeOf[Option[_]]) value
      else {
        if (!value.isEmpty) value.get
        else {
          try{
            defaultVal(p)
          }catch{
            case ex:Throwable=>throw new IllegalArgumentException(s"TypeSafe Error!${method.fullName} ,Argument Name[${p.name.toString}] Is Null!")
          }
        }
      }
    }
    (im.reflectMethod(method))(args:_*).asInstanceOf[A]
  }
  private[this] def defaultVal(symbol:Symbol)={
    symbol.typeSignature match{
      case t if t <:< typeOf[Int]||t<:<typeOf[Double]||t<:<typeOf[Long]||t<:<typeOf[Float]||t<:<typeOf[Char]=>0
      case t if t <:< typeOf[String]=>""
      case t=> throw new IllegalArgumentException("Type Safe Error!")
    }
  }
 }

