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
import au.csiro.data61.magda.model.Auth.User
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ResponseEntity
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.marshalling.ToEntityMarshaller

class RecordsServiceAuthSpec extends ApiSpec {
  override def testConfigSource =
    s"""
       |db.default.url = "${databaseUrl}?currentSchema=test"
       |authorization.skip = false
       |authorization.skipOpaQuery = false
       |opa.recordPolicyId = "default.policy"
    """.stripMargin

  val defaultPolicyResponse = """
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
                        "value": "eq"
                      }
                    ]
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
                        "value": "example"
                      },
                      {
                        "type": "string",
                        "value": "nested"
                      },
                      {
                        "type": "string",
                        "value": "public"
                      }
                    ]
                  },
                  {
                    "type": "string",
                    "value": "true"
                  }
                ]
              }
            ]
          ]
        }
      }
  """

  describe("GET") {
    describe("for a single record") {
      describe("with a default policy") {
        it(
          "allows access to an aspect-less record if default policy resolves to unconditionally allow access"
        ) { param =>
          val recordId = "foo"
          addRecord(param, Record(recordId, "foo", Map()))
          expectOpaQueryForPolicy(param, "default.policy.read", """{
            "result": {
                "queries": []
            }
          }""")

          Get(s"/v0/records/foo") ~> addTenantIdHeader(
            TENANT_1
          ) ~> param.api(Full).routes ~> check {
            status shouldEqual StatusCodes.OK
            val resRecord = responseAs[Record]

            resRecord.id shouldBe "foo"
            resRecord.authnReadPolicyId shouldBe None
          }
        }

        it(
          "disallows access to an aspect-less record if default policy resolves to unconditionally disallow access"
        ) { param =>
          val recordId = "foo"

          addRecord(param, Record(recordId, "foo", Map()))
          expectOpaQueryForPolicy(param, "default.policy.read", """{
            "result": {}
          }""")

          Get(s"/v0/records/foo") ~> addTenantIdHeader(
            TENANT_1
          ) ~> param.api(Full).routes ~> check {
            status shouldEqual StatusCodes.NotFound
          }
        }

        describe("based on the value in an aspect") {
          it(
            "allows access to an record"
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
                )
              )
            )

            expectOpaQueryForPolicy(
              param,
              "default.policy.read",
              defaultPolicyResponse
            )

            Get(s"/v0/records/foo") ~> addTenantIdHeader(
              TENANT_1
            ) ~> param.api(Full).routes ~> check {
              status shouldEqual StatusCodes.OK
            }
          }

          it(
            "denies access to an record"
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
                    "nested" -> JsObject("public" -> JsString("false"))
                  )
                )
              )
            )
            expectOpaQueryForPolicy(
              param,
              "default.policy.read",
              defaultPolicyResponse
            )

            Get(s"/v0/records/foo") ~> addTenantIdHeader(
              TENANT_1
            ) ~> param.api(Full).routes ~> check {
              status shouldEqual StatusCodes.NotFound
            }
          }
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

  def addExampleAspectDef(param: FixtureParam) =
    param.asAdmin(
      Post(
        "/v0/aspects",
        AspectDefinition(
          "example",
          "an example",
          None
        )
      )
    ) ~> addTenantIdHeader(
      TENANT_1
    ) ~> param
      .api(Full)
      .routes ~> check {
      status shouldEqual StatusCodes.OK
    }

  def expectOpaQueryForPolicy(
      param: FixtureParam,
      policyId: String,
      response: String
  ) =
    (param.authFetcher
      .post(
        _: String,
        _: HttpEntity.Strict,
        _: List[HttpHeader]
      )(
        _: ToEntityMarshaller[HttpEntity.Strict]
      ))
      .expects(
        "/opa/compile",
        HttpEntity(
          ContentTypes.`application/json`,
          s"""{
             |  "query": "data.$policyId",
             |  "unknowns": ["input.object"]
             |}""".stripMargin
        ),
        *,
        *
      )
      .returning(
        Marshal(response)
          .to[ResponseEntity]
          .map(
            HttpResponse(
              StatusCodes.OK,
              Nil,
              _
            )
          )
      )

  def addRecord(param: FixtureParam, record: Record) {
    param.asAdmin(Post("/v0/records", record)) ~> addTenantIdHeader(
      TENANT_1
    ) ~> param.api(Full).routes ~> check {
      status shouldEqual StatusCodes.OK
    }
  }
}
