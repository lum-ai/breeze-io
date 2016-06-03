package ser

import java.lang.Float.intBitsToFloat
import java.nio.{ ByteOrder, ByteBuffer }
import java.nio.charset.StandardCharsets

class Npy {

  def read(path: String): NpyTensor = {
    // read bytes
    val bb = readBytes(path)
    // check first byte in magic string
    require(bb.get() == 0x93.toByte, "wrong magic string")
    // check the rest of the magic string
    val magicBytes = new Array[Byte](5)
    bb.get(magicBytes, 0, 5)
    val magicString = new String(magicBytes, StandardCharsets.US_ASCII)
    require(magicString == "NUMPY", "wrong magic string")
    // read version (two unsigned bytes)
    val majorVersion = bb.get() & 0xFF
    val minorVersion = bb.get() & 0xFF
    require(majorVersion == 1 && minorVersion == 0, "only version 1.0 is supported")
    // read header length (little endian)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    val headerLength = bb.getShort()
    // read header
    val headerBytes = new Array[Byte](headerLength)
    bb.get(headerBytes)
    val headerString = new String(headerBytes, StandardCharsets.US_ASCII)
    val header = NpyHeader(headerString)
    require(header.numDims <= 2, "too many dimensions")
    // read data
    val data = readData(header, bb)
    new NpyTensor(header, data)
  }

  def readData(header: NpyHeader, bb: ByteBuffer): Array[_] = {
    bb.order(header.byteOrder)
    header.dtype match {
      case "i1" | "b" =>
        val data = new Array[Byte](header.numElems)
        bb.get(data)
        data
      case "i2" =>
        val dataBuffer = bb.asShortBuffer()
        val data = new Array[Short](header.numElems)
        dataBuffer.get(data)
        data
      case "i4" | "i" =>
        val dataBuffer = bb.asIntBuffer()
        val data = new Array[Int](header.numElems)
        dataBuffer.get(data)
        data
      case "i8" =>
        val dataBuffer = bb.asLongBuffer()
        val data = new Array[Long](header.numElems)
        dataBuffer.get(data)
        data
      case "u1" =>
        val data = new Array[Byte](header.numElems)
        bb.get(data)
        data.map(_ & 0xFF)
      case "u2" =>
        val dataBuffer = bb.asShortBuffer()
        val data = new Array[Short](header.numElems)
        dataBuffer.get(data)
        data.map(_ & 0xFFFF)
      case "u4" =>
        val dataBuffer = bb.asIntBuffer()
        val data = new Array[Int](header.numElems)
        dataBuffer.get(data)
        data.map(_ & 0xFFFFFFFFL)
      case "f2" =>
        val dataBuffer = bb.asShortBuffer()
        val data = new Array[Short](header.numElems)
        dataBuffer.get(data)
        data.map(n => intBitsToFloat(n.toInt))
      case "f4" | "f" =>
        val dataBuffer = bb.asFloatBuffer()
        val data = new Array[Float](header.numElems)
        dataBuffer.get(data)
        data
      case "f8" =>
        val dataBuffer = bb.asDoubleBuffer()
        val data = new Array[Double](header.numElems)
        dataBuffer.get(data)
        data
      // case "u8" => ??? // uint64
      // case "f16" => ??? // float128
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }

}
