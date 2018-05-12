package estimate

import java.text.NumberFormat
import java.util.Locale
import scala.collection.immutable.{Queue, Stack}
import scala.collection.{SortedSet, mutable}
import collection.JavaConverters._
import java.lang.reflect.Modifier
import java.util

object DeepSize {
  private val SKIP_POOLED_OBJECTS: Boolean = false

  private def isPooled(paramObject: AnyRef): Boolean = {
    paramObject match{
      case e: java.lang.Enum[_]   => true
      case s: java.lang.String    => s eq s.intern()
      case b: java.lang.Boolean   => (b eq java.lang.Boolean.TRUE) || (b eq java.lang.Boolean.FALSE)
      case i: java.lang.Integer   => i eq java.lang.Integer.valueOf(i)
      case s: java.lang.Short     => s eq java.lang.Short.valueOf(s)
      case b: java.lang.Byte      => b eq java.lang.Byte.valueOf(b)
      case l: java.lang.Long      => l eq java.lang.Long.valueOf(l)
      case c: java.lang.Character => c eq java.lang.Character.valueOf(c)
      case _ => false
    }
  }

  /**
    * Calculates deep size
    *
    * @param obj
    * object to calculate size of
    * @return object deep size
    */
  def apply(obj: AnyRef): Long = {
    deepSizeOf(obj)
  }

  private def skipObject(obj: AnyRef, previouslyVisited: util.Map[AnyRef, AnyRef]): Boolean = {
    if (SKIP_POOLED_OBJECTS && isPooled(obj)) return true
    (obj == null) || previouslyVisited.containsKey(obj)
  }

  private def deepSizeOf(obj0: AnyRef): Long = {
    val previouslyVisited = new util.IdentityHashMap[AnyRef, AnyRef]
    val objectQueue = mutable.Queue(obj0)
    var current = 0L
    while(objectQueue.nonEmpty){
      val obj = objectQueue.dequeue()
      if (!skipObject(obj, previouslyVisited)){
        previouslyVisited.put(obj, null)
        val thisSize = agent.Agent.getObjectSize(obj)

        // get size of object + primitive variables + member pointers
        // for array header + len + if primitive total value for primitives
        obj.getClass match{
          case a if a.isArray =>
            current += thisSize
            // primitive type arrays has length two, skip them (they included in the shallow size)
            if (a.getName.length != 2) {
              val lengthOfArray = java.lang.reflect.Array.getLength(obj)
              for (i <- 0 until lengthOfArray) {
                objectQueue.enqueue(java.lang.reflect.Array.get(obj, i))
              }
            }
          case c =>
            current += thisSize
            var currentClass: Class[_] = c
            do {
              val objFields = currentClass.getDeclaredFields
              for(field <- objFields) {
                if (
                  !Modifier.isStatic(field.getModifiers) &&
                    !field.getType.isPrimitive
                ) {
                  field.setAccessible(true)
                  var tempObject: AnyRef = null
                  tempObject = field.get(obj)
                  if (tempObject != null) objectQueue.enqueue(tempObject)
                }
              }
              currentClass = currentClass.getSuperclass
            } while (currentClass != null)

        }

      }
    }
    current
  }
}

object MemoryMain{
  def main(args: Array[String]): Unit = {
    def obj = new Object()
    def nums[T](n: Int, f: Int => T) = (0 until n).iterator.map(f)
    val collections = Seq[(String, Int => AnyRef)](
      ("Vector",          nums(_, _ => obj).toVector),
      ("Array",           nums(_, _ => obj).toArray),
      ("List",            nums(_, _ => obj).toList),
      ("UnforcedStream",  nums(_, _ => obj).toStream),
      ("ForcedStream",    {n => val x = nums(n, _ => obj).toStream; x.foreach(x => ()); x}),
      ("Set",             nums(_, _ => obj).toSet),
      ("Map",             nums(_, _ => (obj, obj)).toMap),

      ("SortedSet", nums(_, x=>x).to[SortedSet]),
      ("Queue",     nums(_, _ => obj).to[Queue]),

      ("m.Buffer",    nums(_, _ => obj).to[mutable.Buffer]),
      ("m.Map",       n => mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*)),
      ("m.Set",       nums(_, _ => obj).to[mutable.Set]),
      ("m.Queue",     nums(_, _ => obj).to[mutable.Queue]),
      ("m.PriQueue",  nums(_, x=>x).to[mutable.PriorityQueue]),
      ("m.Stack",     nums(_, _ => obj).to[mutable.Stack]),
      ("m.SortedSet", nums(_, x=>x).to[mutable.SortedSet]),

      ("String",  "1" * _),

      ("ArrayBoolean",  nums(_, _ % 2 == 0).toArray),
      ("ArrayByte",     nums(_, _.toByte).toArray),
      ("ArrayShort",    nums(_, _.toShort).toArray),
      ("ArrayInt",      nums(_, _.toInt).toArray),
      ("ArrayLong",     nums(_, _.toLong).toArray),

      ("BoxArrayBoolean", nums(_, x => (x % 2 == 0).asInstanceOf[AnyRef]).toArray),
      ("BoxArrayByte",    nums(_, _.toByte.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayShort",   nums(_, _.toShort.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayInt",     nums(_, _.toInt.asInstanceOf[AnyRef]).toArray),
      ("BoxArrayLong",    nums(_, _.toLong.asInstanceOf[AnyRef]).toArray),

      ("j.List",    nums(_, _.toLong.asInstanceOf[AnyRef]).toBuffer.asJava: java.util.List[AnyRef]),
      ("j.Map",       n => mutable.Map(nums(n, _ => (obj, obj)).toSeq:_*).asJava: java.util.Map[AnyRef, AnyRef]),
      ("j.Set",     nums(_, _ => obj).to[mutable.Set].asJava: java.util.Set[AnyRef])
    )
    val sizes = Seq(0, 1, 4, 16, 64, 256, 1024, 4069, 16192, 65536, 262144, 1048576)
    val results = for((name, factory) <- collections) yield {
      val numbers = for(n <- sizes) yield DeepSize(factory(n))
      (name, numbers)
    }

    def printRow[I: Integral](name: String, items: Seq[I]) = {
      val width = 15
      println(
        name.padTo(width, ' ') +
          items.map(NumberFormat.getNumberInstance(Locale.US).format)
            .map(_.reverse.padTo(width, ' ').reverse).mkString
      )
    }
    printRow("Size", sizes)
    println()
    for((name, numbers) <- results){
      printRow(name, numbers)
    }
  }
}