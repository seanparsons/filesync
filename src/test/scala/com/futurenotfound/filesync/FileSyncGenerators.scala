package com.futurenotfound.filesync

import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import java.io.File
import java.lang.Boolean
import scalaz._
import Scalaz._
import org.apache.commons.io.FileUtils._
import javax.management.remote.rmi._RMIConnection_Stub

object FileSyncGenerators {

  implicit val fileOrDirectoryGenerator = oneOf(fileOnlyGenerator(getTempFolder()), directoryWithSubElements(getTempFolder(), 10)).map(_.get)

  def fileOnlyGenerator(parent: File): Gen[Option[File]] = for {
    filename <- identifier
    contents <- arbitrary[Array[Byte]]
  } yield buildFile(filename, parent, false, contents.some)

  def directoryGenerator(parent: File): Gen[Option[File]] = for {
    filename <- identifier
  } yield buildFile(filename, parent, true, none)

  def directoryWithSubElements(parent: File, subLevelsAllowed: Int): Gen[Option[File]] = {
    for {
      directory <- directoryGenerator(parent).map(_.get)
      children <- listOfN[Option[File]](10, oneOf(fileOnlyGenerator(directory),
                                    if (subLevelsAllowed > 0) directoryWithSubElements(directory, subLevelsAllowed - 1) else fileOnlyGenerator(directory)))
    } yield buildChildren(directory, children)
  }

  private[this] def buildChildren(parent: File, children: List[Option[File]]) = parent.some

  private[this] def buildFile(filename: String, parent: File, directory: Boolean, contentsOption: Option[Array[Byte]]): Option[File] = {
    val file = new File(parent, filename)
    if (file.exists()) none
    else {
      if (directory) file.mkdir()
      else contentsOption.map(contents => writeByteArrayToFile(file, contents))
      file.deleteOnExit()
      println(file.getAbsolutePath)
      file.some
    }
  }

  private[this] def getTempFolder() = {
    val tempFolder = File.createTempFile("unit", "test")
    tempFolder.delete()
    tempFolder.mkdir()
    tempFolder.deleteOnExit()
    tempFolder
  }
}