package ai.lum.breeze.io

package object npy {

  implicit val byteHandler = new ByteHandler
  implicit val shortHandler = new ShortHandler
  implicit val intHandler = new IntHandler
  implicit val longHandler = new LongHandler
  implicit val floatHandler = new FloatHandler
  implicit val doubleHandler = new DoubleHandler
  implicit val booleanHandler = new BooleanHandler

}
