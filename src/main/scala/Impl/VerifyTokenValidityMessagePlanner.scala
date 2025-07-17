package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.TokenProcess.validateToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import cats.implicits.*
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
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class VerifyTokenValidityMessagePlanner(
                                              userToken: String,
                                              override val planContext: PlanContext
                                            ) extends Planner[Boolean] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[Boolean] = {
    for {
      _ <- IO(logger.info(s"[plan] 开始验证用户 token = ${userToken}"))

      // 调用 validateToken 方法初步验证 token 的有效性
      validationResult <- validateToken(userToken)
      _ <- IO(logger.info(s"[plan] validateToken 验证结果: ${validationResult}"))

      // 如果验证初步无效，直接返回 false，否则进入检查过期时间
      isValid <- if (!validationResult) {
        IO(logger.info(s"[plan] token 验证失败"))
        IO(false)
      } else {
        checkExpiration(userToken)
      }
    } yield isValid
  }

  private def checkExpiration(userToken: String)(using PlanContext): IO[Boolean] = {
    val query =
      s"""
         SELECT expiration_time
         FROM ${schemaName}.user_token_table
         WHERE token = ?;
      """
    val parameters = List(SqlParameter("String", userToken))

    for {
      _ <- IO(logger.info(s"[checkExpiration] 开始检查 token 的过期时间"))

      // 查询数据库获取 token 的过期时间
      expirationTimeOption <- readDBJsonOptional(query, parameters)
      _ <- IO(logger.info(s"[checkExpiration] 数据库查询结果: ${expirationTimeOption}"))

      // 验证过期时间的逻辑
      isValid <- IO {
        expirationTimeOption match {
          case Some(json) =>
            val expirationTime = decodeField[DateTime](json, "expiration_time")
            val currentTime = DateTime.now
            if (expirationTime.isAfter(currentTime)) {
              logger.info(s"[checkExpiration] token 未过期，验证成功。当前时间 = ${currentTime}，token 过期时间 = ${expirationTime}")
              true
            } else {
              logger.info(s"[checkExpiration] token 已过期，验证失败。当前时间 = ${currentTime}，token 过期时间 = ${expirationTime}")
              false
            }
          case None =>
            logger.info(s"[checkExpiration] 在数据库中未找到对应的 token 记录，验证失败")
            false
        }
      }
    } yield isValid
  }
}