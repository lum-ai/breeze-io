package ser

import breeze.linalg._

class NpyTensor(val header: NpyHeader, val data: Array[_]) {

  def toDenseMatrix[A: DataHandler]: DenseMatrix[A] = {
    implicitly[DataHandler[A]].mkDenseMatrix(header, data)
  }

  def toDenseVector[A: DataHandler]: DenseVector[A] = {
    implicitly[DataHandler[A]].mkDenseVector(header, data)
  }

  def toArray[A: DataHandler]: Array[A] = {
    implicitly[DataHandler[A]].mkArray(header, data)
  }

}
