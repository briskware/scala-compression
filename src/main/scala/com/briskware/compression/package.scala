package com.briskware

package object compression {

  type Bit = Boolean
  type Bits = List[Bit]

  /**
    * The binary tree used to represent the encoder and decoder
    */
  sealed trait Tree

  case class Leaf(value: Byte, count: Int) extends Tree {
    override def toString: String = f"Leaf('${value.toChar}'=$value%2X,$count)"
  }

  case class Node(left: Tree, right: Tree) extends Tree


  /**
    * Represents the compressed data in memory
    * @param tree the encoding amd decoding binary tree used to compress/uncompress data
    * @param bits the bit sequence representing the compressed data
    */
  case class CompressedData(tree: Tree, bits: Bits)

  /**
    * Defines the basic Compress behaviour
    */
  trait Compressor extends Logger {

    /**
      * Encode, i.e. compress the passed byte sequence
      * @param bytes
      * @return
      */
    def encodeBytes(bytes: List[Byte]): CompressedData

    /**
      * Decode, i.e. uncompress the passed CompressedData to the original byte sequence
      * @param data
      * @return
      */
    def decodeBits(data: CompressedData): List[Byte]

    /**
      * Logs the passed bit stream as ones and zeroes as they appear in the bit stream.
      * No byte grouping or "endianness" is applied.
      * @param bits
      */
    protected def logBits(bits: Bits): Unit = {
      // dump 8 bytes of binary per line to the console
      log {
        bits.map(b => if (b) '1' else '0')
          .grouped(8).map(_.mkString(""))
          .grouped(8).map(_.mkString(" "))
          .mkString("\n")
      }
    }

    /**
      * Logs the passed bit stream as groups of 4 bits (nibbles) in hexadecimal as they appear in the bit stream.
      * No byte grouping or "endianness" is applied.
      * @param bits
      */
    protected def logHexBits(bits: Bits): Unit = {
      // dump 8 bytes of binary per line to the console
      log {
        bits.map(b => if (b) 1 else 0)
          .grouped(4).map(_.foldRight(0)((l, a) => a * 2 + l))
          .map(b => f"${b.toByte}%1X")
          .grouped(2).map(_.mkString)
          .grouped(16).map(_.mkString(" "))
          .mkString("\n")
      }
    }
  }


  trait Logger {

    /** logs argument
      */
    def log(msg: String): Unit
  }

  trait ConsoleLogger {

    /** logs argument to Console using <code>Console.log</code>
      */
    def log(msg: String): Unit = Console.out.println(msg)
  }

}

//EOF
