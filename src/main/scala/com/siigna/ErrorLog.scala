package com.siigna

import java.io.{FileWriter, File}

import scala.io.Source

object ErrorLog {

  private val tmp = System.getProperty("java.io.tmpdir")
  private val errorDirectory = new File(tmp + File.separator + "repocad_error")

  val getLogger : Option[ErrorLog] = {
    if (!errorDirectory.exists()) {
      errorDirectory.mkdir()
    }
    if (errorDirectory.isDirectory) {
      Some(new ErrorLog(errorDirectory))
    } else {
      None
    }
  }

}

sealed class ErrorLog(home: File) {

  def logError(system: String, data: String): Unit = {
    val errorFile = new File(ErrorLog.errorDirectory + File.separator + system)
    if (!errorFile.isFile) {
      errorFile.createNewFile()
    }

    val fileWriter = new FileWriter(errorFile, true)
    try {
      fileWriter.write(data)
    } finally {
      fileWriter.close()
    }

  }

}
