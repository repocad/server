package com.siigna

import java.io.{File, FileWriter}
import java.nio.file._
import java.util.Date

import scala.io.Source

object Log {

  private val tmp = System.getProperty("java.io.tmpdir")
  private val baseDirectory: String = tmp + File.separator + "repocad" + File.separator

  try {
    val path = Paths.get(baseDirectory)
    if (!Files.isDirectory(path)) {
      Files.createDirectory(path)
    }
  } catch {
    case e: Exception => println("Failed to initialise logger" + e)
  }

  def getLogger(name: String): Option[Log] = {
    try {
      val path = Paths.get(baseDirectory + name)
      if (Files.isDirectory(path)) {
        Some(new Log(path))
      } else {
        val dir = Files.createDirectory(Paths.get(baseDirectory + name))
        Some(new Log(dir))
      }
    } catch {
      case e: Exception => None
    }

  }

}

sealed class Log(home: Path) {

  def getSystem(system: String): Option[Path] = {
    try {
      val path = home.resolve(system)
      if (Files.isRegularFile(path)) {
        Some(path)
      } else {
        Some(Files.createFile(path))
      }
    } catch {
      case e: Exception => println(e)
        None
    }
  }

  def getLog(system: String): Option[String] = {
    getSystem(system).map(_.toFile).map(Source.fromFile).map(_.getLines().mkString("\n"))
  }

  def log(system: String, data: String): Unit = {
    getSystem(system).map(_.toFile).foreach(file => {
      val fileWriter = new FileWriter(file, true)
      try {
        fileWriter.write(new Date().toString)
        fileWriter.write(" - ")
        fileWriter.write(data)
        fileWriter.write("\n")
      } finally {
        fileWriter.close()
      }
    })
  }

}
