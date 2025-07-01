package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.TokenProcess.validateToken
import cats.effect.IO
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits._
import Common.Serialize.CustomColumnTypes.{decodeDateTime, encodeDateTime}
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.DBAPI._
import Common.API.{PlanContext, Planner}
import cats.effect.IO
import Common.Object.SqlParameter
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.ServiceUtils.schemaName
import Utils.TokenProcess.validateToken
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class VerifyTokenValidityMessagePlanner(
                                              userToken: String,
                                              override val planContext: PlanContext
                                            ) extends Planner[Boolean] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[Boolean] = {
    for {
      _ <- IO(logger.info(s"[VerifyTokenValidity] 开始验证Token的有效性: ${userToken}"))
      // Step 1: Validate token and check expiration_time
      isValidToken <- validateToken(userToken)
      _ <- IO(logger.info(s"[VerifyTokenValidity] Token验证结果: ${isValidToken}"))
    } yield isValidToken
  }
}