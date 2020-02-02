package au.csiro.data61.magda.registry

import akka.event.LoggingAdapter
import akka.http.scaladsl.model.StatusCodes
import au.csiro.data61.magda.model.Registry._
import au.csiro.data61.magda.model.TenantId._
import gnieh.diffson._
import gnieh.diffson.sprayJson._
import scalikejdbc.DBSession
import spray.json._
import scalikejdbc.{GlobalSettings, LoggingSQLAndTimeSettings}
import akka.http.scaladsl.marshalling.Marshal

import scala.util.Success
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.HttpResponse
import scala.concurrent.Future
import au.csiro.data61.magda.model.Auth.User
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ResponseEntity
import akka.http.scaladsl.model.ContentTypes

class RecordsServiceAuthSpec extends ApiSpec {
  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = true,
    singleLineMode = true,
    logLevel = 'info
  )

  override def testConfigSource =
    s"""
       |db.default.url = "${databaseUrl}?currentSchema=test"
       |authorization.skip = false
       |authorization.skipOpaQuery = false
       |opa.recordPolicyId = "this.is.a.policy"
    """.stripMargin

  describe("GET") {
    it("allows access to records with no policy") { param =>
      val recordId = "foo"
      val record = Record(recordId, "foo", Map())

      (param.authFetcher
        .get(_: String, _: Seq[HttpHeader]))
        .expects(
          "/opa/compile",
          *,
          HttpEntity(
            ContentTypes.`application/json`,
            """{
              "query": "data.this.is.a.policy.read",
              "unknowns": ["input.object"]
            }"""".stripMargin
          )
        )
        .returning(
          Marshal(User(true))
            .to[ResponseEntity]
            .map(
              HttpResponse(
                StatusCodes.OK,
                Nil,
                _
              )
            )
        )

      // (mockedRecordPersistence
      //   .trimRecordsBySource(
      //     _: SpecifiedTenantId,
      //     _: String,
      //     _: String,
      //     _: Option[LoggingAdapter]
      //   )(_: DBSession))
      //   .expects(*, *, *, *, *)
      //   .onCall {
      //     (
      //         _: SpecifiedTenantId,
      //         _: String,
      //         _: String,
      //         _: Option[LoggingAdapter],
      //         _: DBSession
      //     ) =>
      //       Thread.sleep(600)
      //       Success(1)
      //   }

      param.asAdmin(Post("/v0/records", record)) ~> addTenantIdHeader(
        TENANT_1
      ) ~> param.api(Full).routes ~> check {
        status shouldEqual StatusCodes.OK
      }

      Get(s"/v0/records/foo") ~> addTenantIdHeader(
        TENANT_1
      ) ~> param.api(Full).routes ~> check {
        status shouldEqual StatusCodes.OK
        val resRecord = responseAs[Record]

        resRecord.id shouldBe "foo"
        resRecord.authnReadPolicyId shouldBe None
      }
    }

    // it("if OPA doesn't respond, it should respond as if acecss was denied") {
    //   param =>
    //     val aspectDefinition =
    //       AspectDefinition("auth-facet", "auth-facet", None)
    //     param.asAdmin(Post("/v0/aspects", aspectDefinition)) ~> addTenantIdHeader(
    //       TENANT_1
    //     ) ~> param.api(Full).routes ~> check {
    //       status shouldEqual StatusCodes.OK
    //     }

    //     val recordId = "foo"
    //     val record =
    //       Record(
    //         recordId,
    //         "foo",
    //         Map("auth-facet" -> JsObject(Map("allow" -> JsBoolean(true)))),
    //         authnReadPolicyId = Some("policy")
    //       )

    //     param.asAdmin(Post("/v0/records", record)) ~> addTenantIdHeader(
    //       TENANT_1
    //     ) ~> param.api(Full).routes ~> check {
    //       println(responseAs[String])
    //       status shouldEqual StatusCodes.OK
    //     }

    //     Get(s"/v0/records/foo") ~> addTenantIdHeader(
    //       TENANT_1
    //     ) ~> param.api(Full).routes ~> check {
    //       status shouldEqual StatusCodes.NotFound
    //     }
    // }
  }
}
