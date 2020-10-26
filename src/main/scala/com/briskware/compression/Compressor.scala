package com.briskware.compression

import scala.annotation.tailrec


/**
  * This is an implementation of the Shannon-Fano compression encoding scheme.
  *
  * @see https://users.cs.cf.ac.uk/Dave.Marshall/Multimedia/PDF/09_Basic_Compression.pdf
  */
trait ShannonFano extends Compressor {

  private def makeGroups(bytes: List[Byte]): List[Leaf] = {
    val groups: List[Leaf] = {
      val x = bytes.groupBy(a => a).map {
        case (key, ls) => Leaf(key, ls.length)
      }.toList.sortBy(_.count)

      x
      // Making the number of groups divisible by two has not much effect, commenting out
      /*
      if (x.size % 2 == 1 && x.size < 256) {
        // find a value that does not exist
        val all: Set[Byte] = (0 to 255).map(_.toByte).toSet
        val extra = all.filterNot(x.map(_.value).toSet)
        log(s"Adding padding code: ${extra.head}")
        Leaf(extra.head, 0) :: x
      } else x
      */
    } reverse

    log(s"Number of unique byte values = ${groups.size}")
    groups
  }

  protected def makeTree(ls: List[Leaf]): Tree = {
    if (ls.size > 1) {
      val (left, right) = ls.splitAt(ls.size / 2)
      Node(makeTree(left), makeTree(right))
    } else {
      ls.head
    }
  }

  def logEncodingTree(tree: Tree): Unit = {
    def _inner(_tree: Tree, _code: List[Boolean]): Unit = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false)
        _inner(right, _code :+ true)
      case leaf@Leaf(_, _) =>

        log(s"${_code.map(b => if (b) "1" else "0").mkString("")}\t= ${leaf}")
    }

    _inner(tree, Nil)
  }

  def encoder(tree: Tree): Map[Byte, Bits] = {
    def _inner(_tree: Tree, _code: List[Boolean], _map: Map[Byte, Bits]): Map[Byte, Bits] = _tree match {
      case Node(left, right) =>
        _inner(left, _code :+ false, _inner(right, _code :+ true, _map))
      case Leaf(value, _) =>
        _map + (value -> _code)
    }

    _inner(tree, Nil, Map.empty)
  }

  def encodeBytes(bytes: List[Byte]): CompressedData = {

    val originalByteSize = bytes.size

    log(s"Original ASCII text size = ${originalByteSize} (${originalByteSize * 8} bits)")

    val tree = makeTree(makeGroups(bytes))

    logEncodingTree(tree)

    val enc = encoder(tree)

    val bits = bytes.flatMap { b: Byte => enc(b) }

    val encodedBytesCount = bits.size / 8 + bits.size % 8

    log(s"Encoded bit stream size is ${encodedBytesCount} bytes (${bits.size} bits)")

    log(s"Deflated to ${100 * encodedBytesCount / originalByteSize}% of the original size.")

    logBits(bits)
    logHexBits(bits)

    CompressedData(tree, bits)
  }

  def decodeBits(cData: CompressedData): List[Byte] = {
    def _outer(_bits: Bits, _bytes: List[Byte]): List[Byte] = {
      def _inner(_bits: Bits, _current: Tree): (Bits, Byte) = {
        _current match {
          case Node(left, _) if !_bits.head =>
            //          log(s".bit(${bits.size-_bits.size})=0: ${_current}")
            _inner(_bits.tail, left)
          case Node(_, right) if _bits.head =>
            //          log(s".bit(${bits.size-_bits.size})=1: ${_current}")
            _inner(_bits.tail, right)
          case Leaf(value, _) =>
            (_bits, value)
        }
      }
      if (_bits.isEmpty)
        _bytes
      else {
        val (_b, _byte) = _inner(_bits, cData.tree)
        _outer(_b, _bytes :+ _byte)
      }
    }
    _outer(cData.bits, Nil)
  }

}

//EOF
