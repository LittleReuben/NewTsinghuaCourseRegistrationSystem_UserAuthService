package Impl

/**
 * UserLogoutMessagePlanner: 用户登出操作的实现
 * 输入参数: userToken : String
 * 输出参数: result : String
 */

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
      // Step 1: Log token invalidation progress
      _ <- IO(logger.info(s"[Step 1] 开始注销用户的Token: ${userToken}"))
      
      // Step 2: Perform token invalidation
      isInvalidated <- performTokenInvalidation(userToken)
      _ <- IO(logger.info(s"[Step 2] Token [${userToken}] 是否成功无效化: ${isInvalidated}"))
      
      // Step 3: Return the result based on the invalidation outcome
      resultMessage <- if (isInvalidated) {
        IO(logger.info(s"[Step 3] Token [${userToken}] 已成功登出")) >>
        IO("登出成功！")
      } else {
        IO(logger.warn(s"[Step 3] Token [${userToken}] 登出失败")) >>
        IO("登出失败！")
      }
    } yield resultMessage
  }

  /**
   * Helper function to perform token invalidation using the library method.
   * @param token The user token to invalidate.
   * @return IO[Boolean] indicating whether the invalidation was successful.
   */
  private def performTokenInvalidation(token: String)(using PlanContext): IO[Boolean] = {
    for {
      _ <- IO(logger.info(s"[Helper] 即将调用invalidateToken方法使token无效化: ${token}"))
      isInvalidated <- invalidateToken(token)
      _ <- IO(logger.info(s"[Helper] 调用invalidateToken完成，结果: ${isInvalidated}"))
    } yield isInvalidated
  }
}