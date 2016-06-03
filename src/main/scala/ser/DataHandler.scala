package ser

import scala.reflect._

abstract class DataHandler[A: ClassTag] {

  def convertElement[B](elem: B): A

  def mkArray(header: NpyHeader, data: Array[_]): Array[A] = {
    header.dtype match {
      case "i1" | "b" => data.asInstanceOf[Array[Byte]].map(convertElement)
      case "i2" => data.asInstanceOf[Array[Short]].map(convertElement)
      case "i4" => data.asInstanceOf[Array[Int]].map(convertElement)
      case "i8" => data.asInstanceOf[Array[Long]].map(convertElement)
      case "f4" => data.asInstanceOf[Array[Float]].map(convertElement)
      case "f8" => data.asInstanceOf[Array[Double]].map(convertElement)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }

}

class BooleanHandler extends DataHandler[Boolean] {
  def convertElement[B](elem: B): Boolean = elem != 0
}

class ByteHandler extends DataHandler[Byte] {
  def convertElement[B](elem: B): Byte = elem match {
    case n: Byte => n
    case n: Short => n.toByte
    case n: Int => n.toByte
    case n: Long => n.toByte
    case n: Float => n.toByte
    case n: Double => n.toByte
  }
}

class ShortHandler extends DataHandler[Short] {
  def convertElement[B](elem: B): Short = elem match {
    case n: Byte => n.toShort
    case n: Short => n
    case n: Int => n.toShort
    case n: Long => n.toShort
    case n: Float => n.toShort
    case n: Double => n.toShort
  }
}

class IntHandler extends DataHandler[Int] {
  def convertElement[B](elem: B): Int = elem match {
    case n: Byte => n.toInt
    case n: Short => n.toInt
    case n: Int => n
    case n: Long => n.toInt
    case n: Float => n.toInt
    case n: Double => n.toInt
  }
}

class LongHandler extends DataHandler[Long] {
  def convertElement[B](elem: B): Long = elem match {
    case n: Byte => n.toLong
    case n: Short => n.toLong
    case n: Int => n.toLong
    case n: Long => n
    case n: Float => n.toLong
    case n: Double => n.toLong
  }
}

class FloatHandler extends DataHandler[Float] {
  def convertElement[B](elem: B): Float = elem match {
    case n: Byte => n.toFloat
    case n: Short => n.toFloat
    case n: Int => n.toFloat
    case n: Long => n.toFloat
    case n: Float => n
    case n: Double => n.toFloat
  }
}

class DoubleHandler extends DataHandler[Double] {
  def convertElement[B](elem: B): Double = elem match {
    case n: Byte => n.toDouble
    case n: Short => n.toDouble
    case n: Int => n.toDouble
    case n: Long => n.toDouble
    case n: Float => n.toDouble
    case n: Double => n
  }
}
