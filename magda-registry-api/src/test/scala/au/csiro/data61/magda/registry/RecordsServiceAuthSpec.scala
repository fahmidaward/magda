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

      it(
        "for records with the same policy, denies access to those that don't meet the policy and allows access to those that do"
      ) { param =>
        addExampleAspectDef(param)

        for (i <- 1 to 3) {
          addRecord(
            param,
            Record(
              "allow" + i,
              "allow" + i,
              Map(
                "example" -> JsObject(
                  "nested" -> JsObject("public" -> JsString("true"))
                )
              ),
              authnReadPolicyId = Some("not.default.policyid")
            )
          )
        }

        // Record with the exact path set to false
        addRecord(
          param,
          Record(
            "deny1",
            "deny1",
            Map(
              "example" -> JsObject(
                "nested" -> JsObject("public" -> JsString("false"))
              )
            ),
            authnReadPolicyId = Some("not.default.policyid")
          )
        )

        // Record missing the last value
        addRecord(
          param,
          Record(
            "deny2",
            "deny2",
            Map(
              "example" -> JsObject("nested" -> JsObject())
            ),
            authnReadPolicyId = Some("not.default.policyid")
          )
        )

        // Record with no value for this aspect at all
        addRecord(
          param,
          Record(
            "deny3",
            "deny3",
            Map(),
            authnReadPolicyId = Some("not.default.policyid")
          )
        )

        expectOpaQueryForPolicy(
          param,
          "not.default.policyid.read",
          defaultPolicyResponse
        )

        Get(s"/v0/records") ~> addTenantIdHeader(
          TENANT_1
        ) ~> param.api(Full).routes ~> check {
          status shouldEqual StatusCodes.OK
          val resPage = responseAs[RecordsPage[Record]]

          resPage.records.length shouldBe 3
          resPage.records.forall(_.id.startsWith("allow"))
        }
      }

      it(
        "when records have different policies, displays records with matching policies"
      ) { param =>
        addAspectDef(param, "example1")
        addAspectDef(param, "example2")

        addRecord(
          param,
          Record(
            "allowExample1",
            "allowExample1",
            Map(
              "example1" -> JsObject(
                "nested" -> JsObject("public" -> JsString("true"))
              )
            ),
            authnReadPolicyId = Some("example1.policy")
          )
        )
        addRecord(
          param,
          Record(
            "denyExample1",
            "denyExample1",
            Map(
              "example1" -> JsObject(
                "nested" -> JsObject("public" -> JsString("false"))
              )
            ),
            authnReadPolicyId = Some("example1.policy")
          )
        )
        addRecord(
          param,
          Record(
            "allowExample2",
            "allowExample2",
            Map(
              "example2" -> JsObject(
                "array" -> JsArray(
                  JsNumber(-1),
                  JsNumber(-5),
                  JsNumber(-2)
                )
              )
            ),
            authnReadPolicyId = Some("example2.policy")
          )
        )
        addRecord(
          param,
          Record(
            "denyExample2",
            "denyExample2",
            Map(
              "example2" -> JsObject(
                "array" -> JsArray(
                  JsNumber(-3),
                  JsNumber(2),
                  JsNumber(-4)
                )
              )
            ),
            authnReadPolicyId = Some("example2.policy")
          )
        )

        expectOpaQueryForPolicy(
          param,
          "example1.policy.read",
          defaultPolicyResponse
        )

        expectOpaQueryForPolicy(
          param,
          "example2.policy.read",
          policyResponseWithArray
        )

        Get(s"/v0/records") ~> addTenantIdHeader(
          TENANT_1
        ) ~> param.api(Full).routes ~> check {
          status shouldEqual StatusCodes.OK
          val resPage = responseAs[RecordsPage[Record]]

          resPage.records.length shouldBe 3
          resPage.records.forall(_.id.startsWith("allow"))
        }
      }
    }
  }

  val policyResponseWithArray = """
      {
        "result": {
          "queries": [
            [
              {
                "index": 0,
                "terms": [
                  {
                    "type": "ref",
                    "value": [
                      {
                        "type": "var",
                        "value": "gte"
                      }
                    ]
                  },
                  {
                    "type": "number",
                    "value": 0
                  },
                  {
                    "type": "ref",
                    "value": [
                      {
                        "type": "var",
                        "value": "input"
                      },
                      {
                        "type": "string",
                        "value": "object"
                      },
                      {
                        "type": "string",
                        "value": "registry"
                      },
                      {
                        "type": "string",
                        "value": "record"
                      },
                      {
                        "type": "string",
                        "value": "example2"
                      },
                      {
                        "type": "string",
                        "value": "array"
                      },
                      {
                        "type": "var",
                        "value": "$02"
                      }
                    ]
                  }
                ]
              }
            ]
          ]
        }
      }
  """
}
