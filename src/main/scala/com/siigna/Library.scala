package com.siigna

import java.io.File

import scala.collection.Seq
import scala.io.Source
import scala.util.Right

/**
 * An interface to Library files
 * @param home
 */
sealed case class Library(home : File) {

  def list(path : String) : Either[String, Seq[String]] = {
    val absolutePath = home.getAbsolutePath + File.separator + path
    val file = new File(absolutePath)
    if (file.isDirectory) {
      Right(file.listFiles().map(_.getName))
    } else if (file.isFile) {
      Right(Seq(file.getName))
    } else {
      Left(s"Could not found file $file")
    }
  }

  def absolutePath(path : String) : String = {
    home.getAbsolutePath + File.separator + path
  }

}

/**
 * Helper object to create a library that is guaranteed to be attached to a git repo
 */
object Library {

  val libraryDir = "siigna_library"

  def init() : Library = {
    val tmp = System.getProperty("java.io.tmpdir")
    val dir = tmp + File.separator + libraryDir
    val dirFile = new File(dir)
    if (!dirFile.exists()) {
      dirFile.mkdir()
      clone(new File(tmp))
    } else {
      update(dirFile)
    }
    Library(dirFile)
  }

  private def clone(parent : File) : Unit = {
    val builder = new ProcessBuilder()
    builder.directory(parent)
    builder.command("git", "clone", "git@github.com:siigna/lib.git", libraryDir)
    val p = builder.start()
    p.waitFor()
  }

  private def update(dir : File) : Unit = {
    val builder = new ProcessBuilder()
    builder.directory(dir)
    builder.command("git", "pull")
    val p = builder.start()
    p.waitFor()
  }

}