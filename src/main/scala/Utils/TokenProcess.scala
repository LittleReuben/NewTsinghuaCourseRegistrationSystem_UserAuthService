import Common.API.{PlanContext, Planner}
import Common.DBAPI.{decodeField, readDBRows, writeDBList}
import Common.Object.{SqlParameter, ParameterList}
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import org.joda.time.DateTime

def TokenProcess()(using PlanContext): IO[Unit] = {
  val logger = LoggerFactory.getLogger("TokenProcess")

  for {
    _ <- IO(logger.info("[TokenProcess] 开始处理与Token相关的功能"))

    querySql <- IO {
      s"""
      SELECT token_id, last_used
      FROM ${schemaName}.token
      WHERE status = ? AND expires_at > ?;
      """
    }
    statusParam <- IO(SqlParameter("String", "active"))
    expiresAtParam <- IO(SqlParameter("DateTime", DateTime.now().getMillis.toString))
    
    _ <- IO(logger.info(s"[TokenProcess] 执行查询有效Token的SQL: ${querySql}"))

    tokens <- readDBRows(querySql, List(statusParam, expiresAtParam))
    
    tokenUpdates <- IO {
      tokens.map { json =>
        val tokenId = decodeField[Int](json, "token_id")
        val lastUsed = decodeField[DateTime](json, "last_used")
        logger.info(s"[TokenProcess] Token ID: ${tokenId} 上次使用时间: ${lastUsed}")

        val newLastUsed = DateTime.now()
        ParameterList(List(
          SqlParameter("Int", tokenId.toString),
          SqlParameter("DateTime", newLastUsed.getMillis.toString)
        ))
      }
    }
    
    updateSql <- IO {
      s"""
      UPDATE ${schemaName}.token
      SET last_used = ?
      WHERE token_id = ?;
      """
    }
    
    _ <- tokenUpdates match {
      case Nil => IO(logger.info("[TokenProcess] 无有效Token，无需更新"))
      case updates =>
        IO(logger.info(s"[TokenProcess] 准备更新Token上次使用时间，共更新 ${updates.size} 条记录")) >>
        writeDBList(updateSql, updates).void
    }
    
    _ <- IO(logger.info("[TokenProcess] Token处理完成"))
  } yield ()
}