package au.csiro.data61.magda.registry

import akka.event.LoggingAdapter
import akka.http.scaladsl.model.StatusCodes
import au.csiro.data61.magda.model.Registry._
import au.csiro.data61.magda.model.TenantId._
import gnieh.diffson._
import gnieh.diffson.sprayJson._
import scalikejdbc.DBSession
import spray.json._
import akka.http.scaladsl.marshalling.Marshal

import scala.util.Success
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.HttpResponse
import scala.concurrent.Future
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ResponseEntity
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.marshalling.ToEntityMarshaller

class RecordsServiceAuthSpec extends BaseRecordsServiceAuthSpec {
  override def testConfigSource =
    s"""
       |db.default.url = "${databaseUrl}?currentSchema=test"
       |authorization.skip = false
       |authorization.skipOpaQuery = false
    """.stripMargin

  describe("without a default policy set") {
    commonTests()

    describe("GET") {
      describe("for a single record") {
        it(
          "if there's no default or specific policy in place, it should deny all access"
        ) { param =>
          addExampleAspectDef(param)
          val recordId = "foo"
          addRecord(
            param,
            Record(
              recordId,
              "foo",
              Map(
                "example" -> JsObject(
                  "nested" -> JsObject("public" -> JsString("true"))
                )
              ),
              authnReadPolicyId = None
            )
          )

          Get(s"/v0/records/foo") ~> addTenantIdHeader(
            TENANT_1
          ) ~> param.api(Full).routes ~> check {
            status shouldEqual StatusCodes.NotFound
          }
        }
      }

    }

    describe("for multiple records") {
      it(
        "allows access to aspect-less records if default policy resolves to unconditionally allow access to everything"
      ) { param =>
        val recordId = "foo"

        for (i <- 1 to 5) {
          addRecord(
            param,
            Record(
              recordId + i,
              recordId + i,
              Map(),
              authnReadPolicyId = Some("not.default.policyid")
            )
          )
        }

        expectOpaQueryForPolicy(
          param,
          "not.default.policyid.read",
          """{
            "result": {
                "queries": []
            }
          }"""
        )

        Get(s"/v0/records") ~> addTenantIdHeader(
          TENANT_1
        ) ~> param.api(Full).routes ~> check {
          status shouldEqual StatusCodes.OK
          val resPage = responseAs[RecordsPage[Record]]

          resPage.records.length shouldBe 5
        }
      }

      it(
        "denies access to aspect-less records if default policy resolves to unconditionally deny access to them"
      ) { param =>
        val recordId = "foo"

        for (i <- 1 to 5) {
          addRecord(
            param,
            Record(
              recordId + i,
              recordId + i,
              Map(),
              authnReadPolicyId = Some("not.default.policyid")
            )
          )
        }

        expectOpaQueryForPolicy(
          param,
          "not.default.policyid.read",
          """{
            "result": {}
          }"""
        )

        Get(s"/v0/records") ~> addTenantIdHeader(
          TENANT_1
        ) ~> param.api(Full).routes ~> check {
          status shouldEqual StatusCodes.OK
          val resPage = responseAs[RecordsPage[Record]]

          resPage.records.length shouldBe 0
        }
      }
    }
  }

}
