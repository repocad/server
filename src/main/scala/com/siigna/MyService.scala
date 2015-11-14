package com.siigna

import java.net.URLDecoder
import java.nio.charset.Charset
import java.util.Base64

import akka.actor.Actor
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import spray.http._
import spray.routing._

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

  private val allowOriginHeader = `Access-Control-Allow-Origin`(AllOrigins)
  private val optionsCorsHeaders = List(
    `Access-Control-Allow-Headers`("Origin, X-Requested-With, Content-Type, Accept, Accept-Encoding, Accept-Language, Host, Referer, User-Agent, Access-Control-Allow-Origin"),
    `Access-Control-Max-Age`(1728000))

  def cors[T]: Directive0 = mapRequestContext {
    ctx => ctx.withRouteResponseHandling({
      //It is an option requeset for a resource that responds to some other method
      case Rejected(x) if ctx.request.method.equals(HttpMethods.OPTIONS) && x.exists(_.isInstanceOf[MethodRejection]) => {
        val allowedMethods: List[HttpMethod] = x.collect {
          case rejection: MethodRejection => rejection.supported
        }
        ctx.complete(HttpResponse().withHeaders(
          `Access-Control-Allow-Methods`(OPTIONS, allowedMethods :_*) ::  allowOriginHeader :: optionsCorsHeaders
        ))
      }
    }).withHttpResponseHeadersMapped { headers =>
      allowOriginHeader :: headers
    }
  }

  val drawingsLibrary = Library.init(Master)
  val thumbnailLibrary = Library.init(Thumbnail)

  val myRoute =
    cors {
      path("list" / Rest) { pathRest =>
        get {
          respondWithMediaType(`text/plain`) {
            complete {
              drawingsLibrary.list(pathRest).fold(left => left.toString, right => right.mkString("\n"))
            }
          }
        }
      } ~ path("get" / Rest) { pathRest =>
        val fileName = URLDecoder.decode(pathRest, "utf8")
        getFromFile(drawingsLibrary.absolutePath(fileName))
      } ~ path("update") {
        drawingsLibrary.update() match {
          case 0 => respondWithStatus(StatusCodes.OK) {
            complete("Updated")
          }
          case x => respondWithStatus(StatusCodes.InternalServerError) {
            complete {
              "Error when updating library"
            }
          }
        }
      } ~ path("post" / Rest) { pathRest =>
        post {
          entity(as[String]) { data =>
            val fileName = URLDecoder.decode(pathRest, "utf8")
            complete {
              drawingsLibrary.put(fileName, data.getBytes("utf8")).right.map {
                case 0 => s"$fileName stored successfully"
                case x => s"Unknown error when storing: code $x"
              }.merge
            }
          }
        }
      } ~ path("thumbnail" / Rest) { pathRest =>
        post {
          entity(as[String]) { stringData =>
            val thumbnailName = URLDecoder.decode(pathRest, "utf8") + ".png"
            val base64String = stringData.substring(stringData.indexOf(",") + 1)
            val pngData = Base64.getDecoder.decode(base64String)
            complete {
              thumbnailLibrary.put(thumbnailName, pngData).right.map {
                case 0 => s"$thumbnailName stored successfully"
                case x => s"Unknown error when storing: code $x"
              }.merge
            }
          }
        }
      }
    }
}