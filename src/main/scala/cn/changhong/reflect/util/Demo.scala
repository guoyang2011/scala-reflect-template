package cn.changhong.reflect.util

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>us}
/**
 * Created by yangguo on 15-3-10.
 */
class User{
  var name:String=""
  var pass:String="pass"
  var age:Int=0
}
case class Bean(username:String)
object Demo {
  def main(args: Array[String]): Unit = {
//    init[Bean]()
    System.currentTimeMillis()
    println(instance[Bean].username)
  }

  implicit val mirror=us.runtimeMirror(getClass.getClassLoader)
  def instance[T](implicit t:TypeTag[T],mirror:Mirror):T={
    val clazzMirror=mirror.reflectClass(t.tpe.typeSymbol.asClass)
    val instance=clazzMirror.reflectConstructor(t.tpe.declaration(us.nme.CONSTRUCTOR).asMethod)
    instance("hadoop").asInstanceOf[T]
  }
  def newInstance[T](implicit t:Manifest[T]):T={
    val loader=us.runtimeMirror(getClass.getClassLoader)
    val claaz=loader.reflectClass(typeOf[T].typeSymbol.asClass)
    val m=typeOf[T].declaration(us.nme.CONSTRUCTOR).asMethod
    val claas=claaz.reflectConstructor(m)
    val args=(for(pp<-m.paramss;p<-pp)yield p).foreach{p=>
      println(p.typeSignature+","+p.name+","+p.fullName+","+p.owner)
    }

    claas("hasdsa").asInstanceOf[T]

  }
  def printlnAllMethod[T](implicit t:Manifest[T])= {
    typeOf[T].declarations.filter(s => s.isMethod).foreach { m => println(m.name+","+m.typeSignature)}
  }
  def printAllInnerObj[T](implicit t:Manifest[T]): Unit ={
    typeOf[T].declarations.foreach(o=>println(o.name+","+o.typeSignature))
  }
  def init[T]()(implicit t:scala.reflect.Manifest[T])={
    t.typeArguments.foreach(t=>println(t.runtimeClass+"...."))//.flatMap
  }

}

