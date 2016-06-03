package ser

import breeze.linalg._

trait DataHandler[@specialized(Byte, Short, Int, Long, Float, Double) A] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[A]

  def mkDenseVector(header: NpyHeader, data: Array[_]): DenseVector[A] = {
    require(header.numDims == 1, "wrong number of dimensions")
    val array = mkArray(header, data)
    new DenseVector(mkArray(header, data))
  }

  def mkDenseMatrix(header: NpyHeader, data: Array[_]): DenseMatrix[A] = {
    require(header.numDims == 2, "wrong number of dimensions")
    val array = mkArray(header, data)
    if (header.fortranOrder) {
      // column-major order
      new DenseMatrix(header.shape(0), header.shape(1), array)
    } else {
      // row-major order
      val matrix = new DenseMatrix(header.shape(1), header.shape(0), array)
      matrix.t
    }
  }

}

class ByteHandler extends DataHandler[Byte] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Byte] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]]
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toByte)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toByte)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toByte)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toByte)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toByte)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}

class ShortHandler extends DataHandler[Short] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Short] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toShort)
      case "i2" => data.asInstanceOf[Array[Short]]
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toShort)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toShort)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toShort)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toShort)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}

class IntHandler extends DataHandler[Int] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Int] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toInt)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toInt)
      case "i4" => data.asInstanceOf[Array[Int]]
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toInt)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toInt)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toInt)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}

class LongHandler extends DataHandler[Long] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Long] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toLong)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toLong)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toLong)
      case "i8" => data.asInstanceOf[Array[Long]]
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toLong)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toLong)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}

class FloatHandler extends DataHandler[Float] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Float] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toFloat)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toFloat)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toFloat)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toFloat)
      case "f4" => data.asInstanceOf[Array[Float]]
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toFloat)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}

class DoubleHandler extends DataHandler[Double] {
  def mkArray(header: NpyHeader, data: Array[_]): Array[Double] = {
    header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toDouble)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toDouble)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toDouble)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toDouble)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toDouble)
      case "f8" => data.asInstanceOf[Array[Double]]
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }
}
