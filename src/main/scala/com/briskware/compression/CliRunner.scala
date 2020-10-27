package com.briskware.compression


import java.io.{StringReader, StringWriter}
import java.nio.charset.StandardCharsets

import scala.util.{Success, Try}


object CliRunner extends App  {

  implicit val logger: Logger = Logger.getConsoleLogger(Logger.INFO)

  val compressor = ShannonFano()

  val textToCompress: String = "ACABADADEAABBAAAEDCACDEAAABCDBBEDCBACAE"

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

  logger.info(s"Serialised tree (${serialisedTree.length} bits): ${serialisedTree}")

  //0 0 165 166 0 168 0 167 169
  //0 0 141      142      0 144      0 143      145
  //0 0 11000001 11000010 0 11000100 0 11000011 11000101

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
