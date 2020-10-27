package com.briskware.compression


import java.io.{StringReader, StringWriter}
import java.nio.charset.StandardCharsets

import scala.util.{Success, Try}


object CliRunner extends App  {

  implicit val logger: Logger = Logger.getConsoleLogger(Logger.DEBUG)

  val compressor = ShannonFano()

  val textToCompress: String = "ACABADADEAABBAAAEDCACDEAAABCDBBEDCBACAE"
  val _textToCompress: String =
    """
      |MIT License
      |
      |Copyright (c) 2020 Stefan Szaniszlo
      |
      |Permission is hereby granted, free of charge, to any person obtaining a copy
      |of this software and associated documentation files (the "Software"), to deal
      |in the Software without restriction, including without limitation the rights
      |to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      |copies of the Software, and to permit persons to whom the Software is
      |furnished to do so, subject to the following conditions:
      |
      |The above copyright notice and this permission notice shall be included in all
      |copies or substantial portions of the Software.
      |
      |THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      |IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      |FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      |AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      |LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      |OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      |SOFTWARE.
      |
    """.stripMargin.trim

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val cData: CompressedData = compressor.compress(bytes)

  val decodedBytes = compressor.decompress(cData)

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii") //decodedBytes.map(_.toChar).mkString("[", "", "]")

  assert(decompressedString == textToCompress, "strings don't match")

  logger.info(s"Decompressed content matched with original: ${decompressedString}")

  val sWriter = new StringWriter()

  logger.debug(s"Original tree: ${cData.tree}")

  val io = new CompressorFileIO(cData)
  io.writeTree(sWriter)

  val serialisedTree = sWriter.getBuffer.toString

  //0 0 165       166       0 168       0 167       169
  //0 0 1x41      1x42      0 1x44      0 1x43      1x45
  //0 0 101000001 101000010 0 101000100 0 101000011 101000101

  logger.info(s"Serialised tree (${serialisedTree.length} bits): ${serialisedTree}")

  val sReader = new StringReader(serialisedTree)

  val Success(tree) = CompressorFileIO.readTree(sReader)

  logger.debug(s"Reconstructed tree: ${tree}")

  val fWriter = new StringWriter()
  io.writeAll(fWriter)

  val compressedBitSize = fWriter.getBuffer.toString.length

  val compressedBytesCount = compressedBitSize / 8 + compressedBitSize % 8

  logger.info(s"Overall size is ${compressedBytesCount} bytes (${compressedBitSize} bits): ${fWriter.getBuffer.toString}")

  logger.info(s"Total compressed stream size is ${100*compressedBitSize/bytes.length/8}% of the original.")
}

// EOF
