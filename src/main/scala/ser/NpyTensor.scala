package ser

import breeze.linalg._

class NpyTensor(val header: NpyHeader, val data: Array[_]) {

  def toByteMatrix: DenseMatrix[Byte] = {
    val array: Array[Byte] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]]
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toByte)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toByte)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toByte)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toByte)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toByte)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def toShortMatrix: DenseMatrix[Short] = {
    val array: Array[Short] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toShort)
      case "i2" => data.asInstanceOf[Array[Short]]
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toShort)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toShort)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toShort)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toShort)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def toIntMatrix: DenseMatrix[Int] = {
    val array: Array[Int] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toInt)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toInt)
      case "i4" => data.asInstanceOf[Array[Int]]
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toInt)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toInt)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toInt)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def toLongMatrix: DenseMatrix[Long] = {
    val array: Array[Long] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toLong)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toLong)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toLong)
      case "i8" => data.asInstanceOf[Array[Long]]
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toLong)
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toLong)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def toFloatMatrix: DenseMatrix[Float] = {
    val array: Array[Float] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toFloat)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toFloat)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toFloat)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toFloat)
      case "f4" => data.asInstanceOf[Array[Float]]
      case "f8" => data.asInstanceOf[Array[Double]].map(_.toFloat)
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def toDoubleMatrix: DenseMatrix[Double] = {
    val array: Array[Double] = header.dtype match {
      case "i1" => data.asInstanceOf[Array[Byte]].map(_.toDouble)
      case "i2" => data.asInstanceOf[Array[Short]].map(_.toDouble)
      case "i4" => data.asInstanceOf[Array[Int]].map(_.toDouble)
      case "i8" => data.asInstanceOf[Array[Long]].map(_.toDouble)
      case "f4" => data.asInstanceOf[Array[Float]].map(_.toDouble)
      case "f8" => data.asInstanceOf[Array[Double]]
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
    mkDenseMatrix(array)
  }

  def mkDenseMatrix[A](array: Array[A]): DenseMatrix[A] = {
    require(header.numDims == 2, "wrong number of dimensions")
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
