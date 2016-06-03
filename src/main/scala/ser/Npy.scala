package ser

import java.io._
import java.nio.{ ByteOrder, ByteBuffer }
import java.nio.charset.StandardCharsets
import org.apache.commons.io.IOUtils

class Npy {

  def read(path: String) = {
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
    val data = readData(bb, header)
    new NpyTensor(header, data)
  }

  def readData(bb: ByteBuffer, header: NpyHeader): Array[_] = {
    bb.order(header.byteOrder)
    header.dtype match {
      // case "b" =>
      //   val data = new Array[Byte](header.numElems)
      //   bb.get(data)
      //   data.map(_ != 0)
      case "i1" =>
        val data = new Array[Byte](header.numElems)
        bb.get(data)
        data
      case "i2" =>
        val dataBuffer = bb.asShortBuffer()
        val data = new Array[Short](header.numElems)
        dataBuffer.get(data)
        data
      case "i4" =>
        val dataBuffer = bb.asIntBuffer()
        val data = new Array[Int](header.numElems)
        dataBuffer.get(data)
        data
      case "i8" =>
        val dataBuffer = bb.asLongBuffer()
        val data = new Array[Long](header.numElems)
        dataBuffer.get(data)
        data
      // case "u1" => ??? // uint8
      // case "u2" => ??? // uint16
      // case "u4" => ??? // uint32
      // case "u8" => ??? // uint64
      // case "f2" =>
      //   val dataBuffer = bb.asShortBuffer()
      //   val data = new Array[Short](header.numElems)
      //   dataBuffer.get(data)
      //   data.map(n => Float.intBitsToFloat(n.toInt))
      case "f4" =>
        val dataBuffer = bb.asFloatBuffer()
        val data = new Array[Float](header.numElems)
        dataBuffer.get(data)
        data
      case "f8" =>
        val dataBuffer = bb.asDoubleBuffer()
        val data = new Array[Double](header.numElems)
        dataBuffer.get(data)
        data
      // case "f16" => ??? // float128
      case dtype => throw new Exception(s"unsupported type '$dtype'")
    }
  }


  // FIXME move to package object?
  def readBytes(path: String): ByteBuffer = readBytes(new File(path))

  def readBytes(file: File): ByteBuffer = {
    val in = new FileInputStream(file)
    val array = IOUtils.toByteArray(in)
    in.close()
    ByteBuffer.wrap(array)
  }

}
