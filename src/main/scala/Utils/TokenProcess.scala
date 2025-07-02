package Utils

//process plan import 预留标志位，不要删除
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import org.slf4j.LoggerFactory
import Common.API.{PlanContext, Planner}
import Common.Object.SqlParameter
import cats.effect.IO
import io.circe.Json
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}
import Common.API.PlanContext
import Common.DBAPI.{readDBJsonOptional, decodeField}

case object TokenProcess {
  private val logger = LoggerFactory.getLogger(getClass)
  //process plan code 预留标志位，不要删除
  
  
  def generateToken(userID: Int)(using PlanContext): IO[String] = {
  // val logger = LoggerFactory.getLogger("generateToken")  // 同文后端处理: logger 统一
    logger.info(s"[generateToken] 开始生成用户Token，userID=${userID}")
  
    for {
      // Step 1: 验证userID是否存在
      checkUserSQL <- IO {
        s"""
           SELECT user_id
           FROM ${schemaName}.user_account_table
           WHERE user_id = ?;
         """
      }
      _ <- IO(logger.info(s"[generateToken] 检查用户是否存在，SQL=${checkUserSQL}"))
      userOptional <- readDBJsonOptional(checkUserSQL, List(SqlParameter("Int", userID.toString)))
  
      _ <- userOptional match {
        case None =>
          IO.raiseError(new IllegalArgumentException(s"[generateToken] userID=${userID}不存在"))
        case Some(_) =>
          IO(logger.info(s"[generateToken] userID=${userID}存在"))
      }
  
      // Step 2: 创建Token和过期时间
      token <- IO(java.util.UUID.randomUUID().toString)
      expirationTime <- IO(DateTime.now.plusHours(1)) // Token有效期为1小时
  
      _ <- IO(logger.info(s"[generateToken] 为userID=${userID}生成的token=${token}, 过期时间=${expirationTime}"))
  
      // Step 3: 将Token写入数据库
      insertTokenSQL <- IO {
        s"""
           INSERT INTO ${schemaName}.user_token_table (token, user_id, expiration_time)
           VALUES (?, ?, ?);
         """
      }
      _ <- writeDB(
        insertTokenSQL,
        List(
          SqlParameter("String", token),
          SqlParameter("Int", userID.toString),
          SqlParameter("DateTime", expirationTime.getMillis.toString)
        )
      )
      _ <- IO(logger.info(s"[generateToken] Token写入数据库完成"))
  
    } yield token
  }
  def invalidateToken(userToken: String)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger("invalidateToken")  // 同文后端处理: logger 统一
  
    // SQL statements
    val queryTokenSql = 
      s"""
         SELECT expiration_time
         FROM ${schemaName}.user_token_table
         WHERE token = ?
       """
      
    val invalidateTokenSql =
      s"""
         DELETE FROM ${schemaName}.user_token_table
         WHERE token = ?
       """
  
    for {
      // Step 1: Validate token existence
      _ <- IO(logger.info(s"[Step 1] 检查 token [${userToken}] 是否存在"))
      tokenResult <- readDBJsonOptional(queryTokenSql, List(SqlParameter("String", userToken)))
  
      isInvalidated <- tokenResult match {
        case Some(json) =>
          // Step 2: Check token validity and expiration
          val expirationTime = decodeField[DateTime](json, "expiration_time")
          val currentTime = DateTime.now()
          
          if (expirationTime.isBefore(currentTime)) {
            IO(logger.info(s"[Step 2] token [${userToken}] 已过期，直接从数据库中移除")) >>
            writeDB(invalidateTokenSql, List(SqlParameter("String", userToken))).map(_ => true)
          } else {
            IO(logger.info(s"[Step 2] token [${userToken}] 尚未过期，同样从数据库中移除")) >>
            writeDB(invalidateTokenSql, List(SqlParameter("String", userToken))).map(_ => true)
          }
        case None =>
          IO(logger.info(s"[Step 1] token [${userToken}] 不存在，操作失败")) >>
          IO(false)
      }
  
      _ <- IO(logger.info(s"[结束] Token [${userToken}] 是否已成功使无效: ${isInvalidated}"))
    } yield isInvalidated
  }
  // 修复编译错误的原因: 替换了 `expirationTime` 和 `currentTime` 变量的不正确封装 (去掉了 `IO`) 并保留了正确的调用 `isBefore`。
  
  
  def validateToken(userToken: String)(using PlanContext): IO[Boolean] = {
  // val logger = LoggerFactory.getLogger("TokenValidation")  // 同文后端处理: logger 统一
    for {
      _ <- IO(logger.info(s"[validateToken] 开始验证Token: ${userToken}"))
  
      // 构造 SQL 查询
      querySql <- IO {
        s"""
SELECT token, expiration_time
FROM ${schemaName}.user_token_table
WHERE token = ?;
         """.stripMargin
      }
      parameters <- IO {
        List(SqlParameter("String", userToken))
      }
  
      // 执行数据库操作
      tokenRecordOption <- readDBJsonOptional(querySql, parameters)
      _ <- IO(logger.info(s"[validateToken] 查询结果: ${tokenRecordOption}"))
  
      // 检查Token的有效性
      isValid <- IO {
        tokenRecordOption match {
          case Some(json) =>
            val expirationTime = decodeField[DateTime](json, "expiration_time")
            val currentTime = DateTime.now
            if (expirationTime.isAfter(currentTime)) {
              logger.info(s"[validateToken] Token有效，未过期。Token=${userToken}, 过期时间=${expirationTime}")
              true
            } else {
              logger.info(s"[validateToken] Token无效，已过期。Token=${userToken}, 当前时间=${currentTime}, 过期时间=${expirationTime}")
              false
            }
          case None =>
            logger.info(s"[validateToken] 未找到Token记录，验证失败。Token=${userToken}")
            false
        }
      }
    } yield isValid
  }
}
