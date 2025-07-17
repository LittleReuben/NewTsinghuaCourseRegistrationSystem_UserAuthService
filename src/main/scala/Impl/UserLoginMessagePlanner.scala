package Impl


import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import Utils.TokenProcess.generateToken
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime
import io.circe.Json
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
import Utils.TokenProcess.generateToken
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UserLoginMessagePlanner(
                                    accountName: String,
                                    password: String,
                                    override val planContext: PlanContext
                                  ) extends Planner[String] {

  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[String] = {
    for {
      _ <- IO(logger.info(s"[plan] 验证用户登录信息：账号名 ${accountName}"))

      // 检查账号是否存在，并获取用户 ID 和加密之后的密码
      userRecordJson <- checkAccountExistence()
      userID <- IO { decodeField[Int](userRecordJson, "user_id") }
      hashedPassword <- IO { decodeField[String](userRecordJson, "password") }

      // 验证密码
      _ <- verifyPassword(hashedPassword)
      token <- generateToken(userID) // 注意这里已经将 token 加入了数据库

      _ <- IO(logger.info(s"[plan] 登录成功，返回 token = ${token}"))
    } yield token
  }

  private def checkAccountExistence()(using PlanContext): IO[Json] = {
    val query =
      s"""
         SELECT user_id, password
         FROM ${schemaName}.user_account_table
         WHERE account_name = ?;
       """
    val parameters = List(SqlParameter("String", accountName))

    for {
      _ <- IO(logger.info(s"[checkAccountExistence] 检查账号是否存在，SQL = ${query}"))
      
      userRecordOptional <- readDBJsonOptional(query, parameters)
      userRecord <- userRecordOptional match {
        case Some(record) => IO(record)
        case None =>
          val errorMessage = s"[checkAccountExistence] 账号不存在：账号名 = ${accountName}"
          IO(logger.error(errorMessage)) >>
            IO.raiseError(new IllegalArgumentException(errorMessage))
      }
    } yield userRecord
  }

  private def verifyPassword(hashedPasswordInDB: String)(using PlanContext): IO[Unit] = {
    val hashedPasswordInput = java.security.MessageDigest.getInstance("SHA-256").digest(password.getBytes("UTF-8")).map("%02x".format(_)).mkString

    if (hashedPasswordInDB == hashedPasswordInput) {
      IO(logger.info(s"[verifyPassword] 验证密码成功")).void
    } else {
      val errorMessage = s"[verifyPassword] 账号或密码错误：账号名 = ${accountName}"
      IO(logger.error(errorMessage)) >>
        IO.raiseError(new IllegalArgumentException(errorMessage))
    }
  }
}