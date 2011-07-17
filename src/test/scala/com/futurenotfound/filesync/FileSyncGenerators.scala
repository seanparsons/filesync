package com.futurenotfound.filesync

import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import java.io.File
import java.lang.Boolean
import scalaz._
import Scalaz._
import org.apache.commons.io.FileUtils._

case class GeneratedFile(file: File, subFiles: Set[GeneratedFile]) {
  def this(file: File) = this(file, Set())
}

object FileSyncGenerators {

  implicit val fileOrDirectoryGenerator = oneOf(fileOnlyGenerator(getTempFolder()), directoryWithSubElements(getTempFolder(), 3))

  def fileOnlyGenerator(parent: File): Gen[File] = for {
    filename <- identifier
    contents <- arbitrary[Array[Byte]]
  } yield buildFile(filename, parent, false, contents.some)

  def directoryGenerator(parent: File): Gen[File] = for {
    filename <- identifier
  } yield buildFile(filename, parent, true, none)

  def directoryWithSubElements(parent: File, subLevelsAllowed: Int): Gen[File] = for {
    directory <- directoryGenerator(parent)
    children <- listOfN(10, oneOf(List(fileOnlyGenerator(directory)) ++ (if (subLevelsAllowed > 0) List(directoryWithSubElements(directory, subLevelsAllowed - 1)) else List())))
  } yield directory

  private[this] def buildFile(filename: String, parent: File, directory: Boolean, contentsOption: Option[Array[Byte]]): File = {
    val file = new File(parent, filename)
    if (directory) file.mkdir()
    else contentsOption.map(contents => writeByteArrayToFile(file, contents))
    file.deleteOnExit()
    file
  }

  private[this] def getTempFolder() = {
    val tempFolder = File.createTempFile("unit", "test")
    tempFolder.mkdir()
    tempFolder.deleteOnExit()
    tempFolder
  }
}