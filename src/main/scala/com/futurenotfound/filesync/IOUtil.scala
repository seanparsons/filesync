package com.futurenotfound.filesync

import java.io.{File, Closeable}

object IOUtil {
  def using[T, U <: Closeable](resource: U)(expression: (U) => T): T = {
    try {
      expression(resource)
    } finally {
      resource.close()
    }
  }

  def getAllFiles(file: File): Set[File] = {
    if (file.isFile) Set(file)
    else file.listFiles().flatMap(file => getAllFiles(file)).toSet + file
  }
}