package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.TokenProcess.invalidateToken
import cats.effect.IO
import org.slf4j.LoggerFactory
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
import Utils.TokenProcess.invalidateToken
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UserLogoutMessagePlanner(
                                      userToken: String,
                                      override val planContext: PlanContext
                                    ) extends Planner[String] {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[String] = {
    for {
      _ <- IO(logger.info(s"[plan] 开始注销用户的 token = ${userToken}"))

      isInvalidated <- invalidateToken(userToken)
      _ <- IO(logger.info(s"[plan] 调用 invalidateToken 完成，结果 = ${isInvalidated}"))

      resultMessage <- if (isInvalidated) {
        IO(logger.info(s"[plan] token ${userToken} 已成功登出")) >>
        IO("登出成功！")
      } else {
        IO(logger.warn(s"[plan] token ${userToken} 登出失败")) >>
        IO("登出失败！")
      }
    } yield resultMessage
  }
}