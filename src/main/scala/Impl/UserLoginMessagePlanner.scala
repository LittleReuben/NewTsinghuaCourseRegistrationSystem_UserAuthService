package Impl


import Utils.TokenProcess.generateToken
import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import io.circe.{Json, Decoder}
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
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using PlanContext): IO[String] = {
    for {
      // Step 1: 检查账号是否存在
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 检查账号是否存在，accountName=${accountName}"))
      userOptional <- isAccountExist(accountName)

      user <- userOptional match {
        case Some(userJson) =>
          IO(logger.info(s"[UserLoginMessagePlanner] 检查到账号存在，accountName=${accountName}")) >> IO(userJson)
        case None =>
          IO(logger.info(s"[UserLoginMessagePlanner] 账号不存在，accountName=${accountName}")) >>
            IO.raiseError(new IllegalArgumentException("该账号不存在"))
      }

      // Step 2: 验证密码
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 验证密码"))
      isValid <- validatePassword(password, user)

      _ <- if (!isValid) {
        IO(logger.info(s"[UserLoginMessagePlanner] 账号或密码错误，accountName=${accountName}")) >>
          IO.raiseError(new IllegalArgumentException("账号或密码错误"))
      } else IO.unit

      // Step 3: 生成Token
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 开始生成Token"))
      userID <- IO(decodeField[Int](user, "user_id"))
      token <- generateToken(userID)

      // Step 4: 写入Token到数据库
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 将Token写入数据库"))
      expirationTime = DateTime.now.plusHours(2) // Token有效期为2小时
      _ <- writeTokenToDB(token, userID, expirationTime)

    } yield token
  }

  private def isAccountExist(accountName: String)(using PlanContext): IO[Option[Json]] = {
    for {
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 查询用户账号信息"))
      sql <- IO {
        s"""
          SELECT user_id, password, salt
          FROM ${schemaName}.user_account_table
          WHERE account_name = ?;
        """
      }
      result <- readDBJsonOptional(sql, List(SqlParameter("String", accountName)))
    } yield result
  }

  private def validatePassword(inputPassword: String, userJson: Json)(using PlanContext): IO[Boolean] = {
    for {
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 验证输入密码是否正确"))
      storedHash <- IO(decodeField[String](userJson, "password"))
      salt <- IO(decodeField[String](userJson, "salt"))
      hashedInputPassword <- IO(hashPassword(inputPassword, salt))
    } yield hashedInputPassword == storedHash
  }

  private def hashPassword(password: String, salt: String): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    val saltedPassword = password + salt
    val hashBytes = digest.digest(saltedPassword.getBytes)
    hashBytes.map("%02x".format(_)).mkString
  }

  private def writeTokenToDB(token: String, userID: Int, expirationTime: DateTime)(using PlanContext): IO[Unit] = {
    for {
      _ <- IO(logger.info(s"[UserLoginMessagePlanner] 开始执行写入Token的数据库操作"))
      sql <- IO {
        s"""
          INSERT INTO ${schemaName}.user_token_table (token, user_id, expiration_time)
          VALUES (?, ?, ?);
        """
      }
      _ <- writeDB(
        sql,
        List(
          SqlParameter("String", token),
          SqlParameter("Int", userID.toString),
          SqlParameter("Long", expirationTime.getMillis.toString)
        )
      ).void
    } yield ()
  }
}