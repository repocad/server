package com.siigna


import java.net.URLDecoder

import akka.actor.Actor
import spray.http.HttpHeaders.{`Access-Control-Allow-Credentials`, `Access-Control-Allow-Origin`}
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
        respondWithHeaders(`Access-Control-Allow-Origin`(AllOrigins), `Access-Control-Allow-Credentials`(true)) {
          respondWithMediaType(`text/plain`) {
            complete {
              library.list(pathRest).fold(left => left.toString, right => right.mkString("\n"))
            }
          }
        }
      }
    } ~ path ("get" / Rest) { pathRest =>
      respondWithHeaders(`Access-Control-Allow-Origin`(AllOrigins), `Access-Control-Allow-Credentials`(true)) {
        val fileName = URLDecoder.decode(pathRest, "utf8")
        getFromFile(library.absolutePath(fileName))
      }
    } ~ path ("update") {
      library.update() match {
        case 0 => respondWithStatus(StatusCodes.OK) { complete("Updated") }
        case x => respondWithStatus(StatusCodes.InternalServerError) {
          complete {
            "Error when updating library"
          }
        }
      }
    } ~ path ("post" / Rest) { pathRest =>
      post {
        entity(as[String]) { data =>
          val fileName = URLDecoder.decode(pathRest, "utf8")
          respondWithHeaders(`Access-Control-Allow-Origin`(AllOrigins), `Access-Control-Allow-Credentials`(true)) {
            complete {
              library.put(fileName, data).right.map {
                case 0 => s"$fileName stored successfully"
                case x => s"Unknown error when storing: code $x"
              }.merge
            }
          }
        }
      }
    }
}