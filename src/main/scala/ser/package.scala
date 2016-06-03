import java.io._
import java.nio.ByteBuffer
import org.apache.commons.io.IOUtils

package object ser {

  implicit val byteHandler = new ByteHandler
  implicit val shortHandler = new ShortHandler
  implicit val intHandler = new IntHandler
  implicit val longHandler = new LongHandler
  implicit val floatHandler = new FloatHandler
  implicit val doubleHandler = new DoubleHandler

  def readBytes(path: String): ByteBuffer = readBytes(new File(path))

  def readBytes(file: File): ByteBuffer = {
    val in = new FileInputStream(file)
    val array = IOUtils.toByteArray(in)
    in.close()
    ByteBuffer.wrap(array)
  }

}
