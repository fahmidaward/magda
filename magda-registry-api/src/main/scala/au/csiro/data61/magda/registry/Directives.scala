package au.csiro.data61.magda.registry

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.Materializer
import au.csiro.data61.magda.directives.AuthDirectives
import au.csiro.data61.magda.opa.OpaTypes._
import com.typesafe.config.Config
import scalikejdbc.DB

import scala.concurrent.{ExecutionContext, Future}

object Directives {

  def withRecordOpaQuery(
      operationType: AuthOperations.OperationType,
      recordPersistence: RecordPersistence,
      authApiClient: RegistryAuthApiClient,
      recordId: Option[String] = None
  )(
      implicit config: Config,
      system: ActorSystem,
      materializer: Materializer,
      ec: ExecutionContext
  ): Directive1[List[(String, List[List[OpaQuery]])]] = {
    AuthDirectives.getJwt().flatMap { jwt =>
      val dbPolicyIds = DB readOnly { session =>
        recordPersistence.getPolicyIds(session, operationType, recordId)
      } get
      val recordFuture =
        authApiClient.queryForRecords(jwt, operationType, dbPolicyIds)

      onSuccess(recordFuture).flatMap { queryResults =>
        provide(queryResults)
      }
    }
  }
}
