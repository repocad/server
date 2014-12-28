package com.siigna

import java.io.File

import akka.actor.Actor
import spray.http.HttpHeaders.{RawHeader, `Access-Control-Allow-Credentials`, `Access-Control-Allow-Origin`}
import spray.routing._
import spray.http._
import MediaTypes._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}

// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

  val library = Library.init()

  val myRoute =
    path("list" / Rest) { pathRest =>
      get {
        respondWithMediaType(`text/plain`) {
          complete {
            library.list(pathRest).fold(left => left.toString, right => right.mkString("\n"))
          }
        }
      }
    } ~ path ("get" / Rest) { pathRest =>
      getFromFile(library.absolutePath(pathRest))
    }
}