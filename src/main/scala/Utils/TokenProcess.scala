import Common.API.{PlanContext, Planner}
import Common.DBAPI._
import Common.Object.ParameterList
import Common.Object.SqlParameter
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime

def TokenProcess()(using PlanContext): IO[Unit] = {
  val logger = LoggerFactory.getLogger("TokenProcess")

  for {
    // Step 1: Log start of token processing
    _ <- IO(logger.info("[TokenProcess] 开始处理 Token 相关的功能"))

    // Step 2: Prepare SQL queries
    querySQL <- IO {
      s"SELECT token_id, expiration_time FROM ${schemaName}.tokens WHERE is_active = true"
    }
    updateSQL <- IO {
      s"UPDATE ${schemaName}.tokens SET is_active = false WHERE token_id = ?"
    }
    _ <- IO(logger.info(s"[TokenProcess] 进行活跃 Token 查询，SQL语句: ${querySQL}"))

    // Step 3: Retrieve active tokens from database
    activeTokens <- readDBRows(querySQL, List.empty)

    // Step 4: Filter expired tokens
    expiredTokens <- IO {
      logger.info(s"[TokenProcess] 共获取到 ${activeTokens.size} 个活跃 Token")
      activeTokens.filter { json =>
        val expirationTime = decodeField[DateTime](json, "expiration_time")
        val isExpired = expirationTime.isBefore(DateTime.now())
        if (isExpired) {
          val tokenId = decodeField[Int](json, "token_id")
          logger.info(s"[TokenProcess] 检测到过期Token ID: ${tokenId}, 到期时间: ${expirationTime}")
        }
        isExpired
      }
    }

    _ <- IO(logger.info(s"[TokenProcess] 检测到 ${expiredTokens.size} 个需要禁用的过期 Token"))

    // Step 5: Prepare parameters for database update
    updateParams <- IO {
      expiredTokens.map { token =>
        val tokenId = decodeField[Int](token, "token_id")
        ParameterList(List(SqlParameter("Int", tokenId.toString)))
      }
    }

    // Step 6: Perform batch update if necessary
    _ <- if (updateParams.nonEmpty) {
      IO(logger.info("[TokenProcess] 批量更新过期 Token")) >>
      writeDBList(updateSQL, updateParams).flatMap { result =>
        IO(logger.info(s"[TokenProcess] 所有过期 Token 更新完成, 数据库反馈: ${result}"))
      }
    } else {
      IO(logger.info("[TokenProcess] 当前没有需要更新的过期 Token，跳过批量更新"))
    }

    // Step 7: Log completion of token processing
    _ <- IO(logger.info("[TokenProcess] Token 相关处理完成"))
  } yield ()
}