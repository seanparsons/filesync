package com.futurenotfound.filesync

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import FileSyncGenerators._
import IOUtil._

case object FileTreeProperties extends Properties("FileTree") {
  property("File.exists") = forAll(fileOrDirectoryGenerator){ (file) =>
    getAllFiles(file).forall(file => file.exists)
  }
}