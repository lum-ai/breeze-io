package ai.lum.breeze.io.npy

import breeze.linalg._

class NpyTensor(val header: NpyHeader, val data: Array[_]) {

  def toArray[A: DataHandler]: Array[A] = {
    implicitly[DataHandler[A]].mkArray(header, data)
  }

  def isVector: Boolean = header.numDims == 1

  def isMatrix: Boolean = header.numDims == 2

  def toDenseVector[A: DataHandler]: DenseVector[A] = {
    require(isVector, "wrong number of dimensions")
    val array = implicitly[DataHandler[A]].mkArray(header, data)
    new DenseVector(array)
  }

  def toDenseMatrix[A: DataHandler]: DenseMatrix[A] = {
    require(isMatrix, "wrong number of dimensions")
    val array = implicitly[DataHandler[A]].mkArray(header, data)
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
