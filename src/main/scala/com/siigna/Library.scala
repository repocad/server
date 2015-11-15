package com.siigna

import java.io.{FileOutputStream, File}

import scala.collection.{JavaConversions, Seq}
import scala.util.Right

/**
 * An interface to Library files
 * @param home  The home directory as a file
 */
sealed case class Library(home : File) {

  private val lock = new Object()

  def absolutePath(path : String) : String = {
    home.getAbsolutePath + File.separator + path
  }

  private def createFile(file : File): Either[String, File] = {
    if (file.exists()) {
      Right(file)
    } else {
      try {
        file.createNewFile()
        Right(file)
      } catch {
        case e : Throwable => Left(e.getLocalizedMessage)
      }
    }
  }

  private def commit(file : File) : Int = {
    lock.synchronized {
      run("git", "add", file.toString)
      run("git", "commit", "-m", "Web editor commit")
    }

    push()
  }
  private def merge() : Int = {
    lock.synchronized {
      run("git", "merge", "branch", "-X", "ours")
    }
  }

  private def push() : Int = {
    update() match {
      case 0 => 0
      case _ => merge()
    }
    run("git", "push")
  }

  private def run(args : String*) : Int = {
    Library.run(home, args)
  }

  def update() : Int = {
    lock.synchronized {
      run("git", "pull")
    }
  }

  def list(path : String) : Either[String, Seq[String]] = {
    val absolutePath = home.getAbsolutePath + File.separator + path
    val file = new File(absolutePath)
    if (file.isDirectory) {
      Right(file.listFiles()
        .filter(f => !(f.getName.startsWith(".") || f.getName.endsWith(".md")))
        .map(file => {
        if (file.isDirectory) {
          file.getName + File.separator
        } else {
          file.getName
        }
      }))
    } else if (file.isFile) {
      Right(Seq(file.getName))
    } else {
      Left(s"Could not found file $file")
    }
  }

  def put(name : String, data : Array[Byte]) : Either[String, Int] = {
    createFile(new File(home.getAbsolutePath + File.separator + name))
      .right.flatMap(f => writeToFile(f, data))
      .right.map(f => commit(f)).right.flatMap {
      case 0 => Right(0)
      case x => Left(s"Unknown error while pushing ")
    }
  }

  private def writeToFile(file : File, data : Array[Byte]) : Either[String, File] = {
    try {
      val output = new FileOutputStream(file)
      output.write(data)
      output.close()
      Right(file)
    } catch {
      case e : Throwable => Left(e.getLocalizedMessage)
    }
  }

}

/**
 * Helper object to create a library that is guaranteed to be attached to a git repo
 */
object Library {

  private val tmp = System.getProperty("java.io.tmpdir")
  private val libraryDir = new File(tmp + File.separator + "repocad_lib")

  def init(branch : Branch) : Library = {
    val libraryFile = new File(libraryDir + File.separator + branch.name)
    if (!libraryDir.exists()) {
      libraryDir.mkdir()
    }

    if (!libraryFile.exists()) {
      clone(branch)
    }

    val library = new Library(libraryFile)
    library.update()
    library
  }

  private[Library] def run(dir : File, args : Seq[String]) : Int = {
    val builder = new ProcessBuilder()
    builder.directory(dir)
    builder.command(JavaConversions.seqAsJavaList(args))
    builder.inheritIO()
    val p = builder.start()
    p.waitFor()
  }

  private def clone(branch : Branch) : Int = {
    run(libraryDir, Seq("git", "clone", "-b", branch.name, "git@github.com:repocad/lib.git", branch.name))
  }

}

abstract class Branch(val name : String)
case object Master extends Branch("master")
case object Thumbnail extends Branch("thumbnail")