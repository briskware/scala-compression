package com.briskware.compression

import java.io._

import scala.annotation.tailrec
import scala.util.Try

object CompressorFileIO {

  def readTree(reader: Reader)(implicit logger: Logger): Try[Tree] = {

    def _inner(_tree: Tree): Tree = {
      val byte = reader.read()
      if (byte == -1) {
        _tree
      } else {
        val bit: Boolean = byte.toChar == '1'
        if (bit) {
          // leaf
          val code: String = Stream.continually(reader.read()).takeWhile(_ != -1).map(_.toChar).take(8).mkString
          val value: Byte = code.foldLeft(0)((a, l) => a * 2 + l - '0').toByte
          val leaf = Leaf(value, -1)
          logger.trace(f"L[x${value}%02X=${code}] = ${leaf}")
          leaf
        } else {
          //node
          logger.trace("N")
          Node(_inner(_tree), _inner(_tree))
        }
      }
    }

    Try(_inner(null))
  }

  def fromFile(file: File): CompressedData = {
    ???
  }
}

class CompressorFileIO(data: CompressedData)(implicit logger: Logger) {

  def writeAll(os: Writer): Try[Boolean] = {
    for {
      //os <- Try(new FileWriter(file))
      hSum <- writeHeader(os)
      t <- writeTree(os)
      b <- writeBody(os)
      c <- writeChecksum(os)
      e <- Try(os.close())
    } yield {
      true
    }
  }

  protected def writeHeader(os: Writer): Try[Long] = {
    for {
      marker <- Try(os.write("BWSFZ"))
    } yield {
      0L
    }
  }

   def writeTree(os: Writer): Try[Boolean] = {

    Try {
      def _inner(tree: Tree): Unit = tree match {
        case Node(left, right) =>
          logger.trace("N")
          os.write("0")
          _inner(left)
          _inner(right)
        case Leaf(value,_) =>
          val binStr = f"${value.toBinaryString}%8s".replaceAll(" ", "0")
          logger.trace(f"L[x${value}%02x=$binStr]")
          os.write(s"1${binStr}")
      }
      _inner(data.tree)
      true
    }
  }


  protected def writeBody(os: Writer): Try[Boolean] = {
    Try {
      @tailrec
      def _inner(bits: Bits): Unit = {
        if (bits.nonEmpty) {
          if(bits.head) os.write('1') else os.write('0')
          _inner(bits.tail)
        }
      }
      _inner(data.bits)
      true
    }
  }

  protected def writeChecksum(os: Writer): Try[Boolean] = ???

  private def writeBits(os: Writer, bs: Bits): Try[Int] = {
    ???
  }

  def getChecksum(): Try[Bits] = ???

  def validateChecksum(): Try[Boolean] = ???
}

//EOF
