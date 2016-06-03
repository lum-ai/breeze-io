package ser

import breeze.linalg._

class NpyTensor(val header: NpyHeader, val data: Array[_]) {

  def toDenseMatrix[@specialized(Byte, Short, Int, Long, Float, Double) A: DataHandler]: DenseMatrix[A] = {
    implicitly[DataHandler[A]].mkDenseMatrix(header, data)
  }

  def toDenseVector[@specialized(Byte, Short, Int, Long, Float, Double) A: DataHandler]: DenseVector[A] = {
    implicitly[DataHandler[A]].mkDenseVector(header, data)
  }

  def toArray[@specialized(Byte, Short, Int, Long, Float, Double) A: DataHandler]: Array[A] = {
    implicitly[DataHandler[A]].mkArray(header, data)
  }

}
