package com.briskware.compression

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets

import com.briskware.compression.compression.Bits

import scala.util.Try

package object compression {
  type Bit = Boolean
  type Bits = List[Bit]
}

/**
  * @see https://users.cs.cf.ac.uk/Dave.Marshall/Multimedia/PDF/09_Basic_Compression.pdf
  */
object ShannonFano extends App with Compress {
  val _textToCompress: String = "Hello"
  val textToCompress: String =
   s"""|// Traditional Struct
       |struct Color {
       |    red: u8,
       |    green: u8,
       |    blue: u8
       |}
       |
       |// Tuple Struct
       |struct TColor(u8, u8, u8);
       |
       |struct Person {
       |    first_name: String,
       |    last_name: String
       |}
       |
       |impl Person {
       |    // Constructor
       |    fn new(first: &str, last: &str) -> Person {
       |        Person {
       |            first_name: first.to_string(),
       |            last_name: last.to_string()
       |        }
       |    }
       |
       |    fn full_name(&self) -> String {
       |        format!("{} {}", self.first_name, self.last_name)
       |    }
       |
       |    fn set_last_name(&mut self, last: &str) {
       |        self.last_name = last.to_string();
       |    }
       |
       |    fn to_tuple(&self) -> (&str, &str) {
       |        (self.first_name.as_str(), self.last_name.as_str())
       |    }
       |}
       |
       |
       |pub fn run() {
       |    let mut color = Color {
       |        red: 255, green: 0, blue: 0
       |    };
       |
       |    color.blue = 128;
       |    println!("Color: {} {} {}", color.red, color.green, color.blue);
       |
       |    let mut tcolor = TColor(255, 0, 0);
       |
       |    tcolor.2 = 128;
       |    println!("TColor: {} {} {}", tcolor.0, tcolor.1, tcolor.2);
       |
       |    let s = Person::new("Stefan", "Szaniszlo");
       |    println!("Person: {} {}", s.first_name, s.last_name);
       |    println!("Person: {}", s.full_name());
       |
       |    let mut p = Person::new("Theresa", "May");
       |    println!("Person: {}", p.full_name());
       |    p.set_last_name("Johnson");
       |    println!("Person: {}", p.full_name());
       |    println!("Person: {:?}", p.to_tuple())
       |
 |}
    """.stripMargin

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val (tree, bits) = encodeBytes(bytes)

  val decodedBytes = decodeBits(bits, tree)

  println("Decompressed content follows below:")

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii")//decodedBytes.map(_.toChar).mkString("[", "", "]")

  println(decompressedString)

  assert(decompressedString == textToCompress, "strings don't match")
}


object CompressedData {
  def fromStream: CompressedData = {
    ???
  }
}

case class CompressedData(tree: Tree, bits: Bits) {

  def toFile(file: File): Try[Boolean] = {
    val out = Try(new FileOutputStream(file))
    for {
      os <- out
      h <- writeHeader(os)
      b <- writeBody(os)
      c <- writeChecksum(os)
      e <- Try(os.close())
    } yield {
      true
    }
  }

  protected def writeHeader(os: FileOutputStream): Try[Boolean] = {
    Try {
      true
    }
  }
  protected def writeBody(os: FileOutputStream): Try[Boolean] = ???
  protected def writeChecksum(os: FileOutputStream): Try[Boolean] = ???

  private def writeBits(os: FileOutputStream, bs: Bits): Try[Int] = {
    ???
  }

  def getChecksum(): Try[Bits] = ???
  def validateChecksum(): Try[Boolean] = ???
}


trait Compress {


  private def makeGroups(bytes: List[Byte]): List[Leaf] = {
    val groups: List[Leaf] = {
      val x = bytes.groupBy(a => a).map {
        case (key, ls) => Leaf(key, ls.length)
      }.toList.sortBy(_.count)

      if (x.size % 2 == 1 && x.size < 256) {
        // find a value that does not exist
        val all: Set[Byte] = (0 to 255).map(_.toByte).toSet
        val extra = all.filterNot(x.map(_.value).toSet)
        println(s"Adding padding code: ${extra.head}")
        Leaf(extra.head, 0) :: x
      } else x
    } reverse

    val uniq = groups.size

    println(s"Unique ASCII characters = ${uniq}")
    //println()
    //println(groups)

    groups
  }

  private def makeTree(ls: List[Leaf]): Tree = {
    if (ls.size > 1) {
      val (left, right) = ls.splitAt(ls.size / 2)
      Node(makeTree(left), makeTree(right))
    } else {
      ls.head
    }
  }

  def printCodingTree(tree: Tree): Unit = {
    def _inner(_tree: Tree, _code: List[Boolean]): Unit = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false)
        _inner(right,_code :+ true)
      case leaf@Leaf(_, _) =>
        println(s"${_code.map(b => if (b) "1" else "0").mkString("")}\t= ${leaf}")
    }
    _inner(tree, Nil)
  }

  def encoder(tree: Tree): Map[Byte, Bits] = {
    def _inner(_tree: Tree, _code: List[Boolean], _map: Map[Byte, Bits]): Map[Byte, Bits] = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false, _inner(right, _code :+ true, _map))
      case Leaf(value, _) =>
        _map + ( value -> _code)

    }
    _inner(tree, Nil, Map.empty)
  }

  def encodeBytes(bytes: List[Byte]): (Tree, Bits) = {

    val originalByteSize = bytes.size

    println(s"Original ASCII text size = ${originalByteSize} (${originalByteSize*8} bits)")

    val tree = makeTree(makeGroups(bytes))

    printCodingTree(tree)

    val enc = encoder(tree)

    val bits = bytes.flatMap { b: Byte => enc(b) }

    val encodedBytesCount = bits.size/8 + bits.size%8

    println(s"Encoded bit stream size is ${encodedBytesCount} bytes (${bits.size} bits)")

    println(s"Deflated to ${100*encodedBytesCount/originalByteSize}% of the original size.")

    println(bits.map(b => if(b) '1' else '0').grouped(8).map(_.mkString("")).grouped(8).map(_.mkString(" ")).mkString("\n"))

    (tree, bits)
  }

  def decodeBits(bits: Bits, tree: Tree): List[Byte] = {

    def _inner(_bits: Bits, _current: Tree): (Bits, Byte) = {

      //assert(_bits.nonEmpty, "premature end of bit stream detected")


      _current match {
        case Node(left, _) if !_bits.head =>
//          println(s".bit(${bits.size-_bits.size})=0: ${_current}")
          _inner(_bits.tail, left)
        case Node(_, right) if _bits.head =>
//          println(s".bit(${bits.size-_bits.size})=1: ${_current}")
          _inner(_bits.tail, right)
        case Leaf(value, _) =>
          (_bits, value)
      }
    }

    def _outer(_bits: Bits, _bytes: List[Byte]): List[Byte] = {
      if (_bits.isEmpty)
        _bytes
      else {
        val (_b, _byte) = _inner(_bits, tree)
        _outer(_b, _bytes :+ _byte)
      }
    }

    _outer(bits, Nil)
  }

}

sealed trait Tree
case class Leaf(value: Byte, count: Int) extends Tree {
  override def toString: String = s"Leaf('${value.toChar}'=$value,$count)"
}
case class Node(left: Tree, right: Tree) extends Tree

