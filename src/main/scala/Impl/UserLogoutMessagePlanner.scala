package Impl


/**
 * Planner for UserLogoutMessage: 用户传入token完成登出操作。
 */
import Utils.TokenProcess.invalidateToken
import Common.API.{PlanContext, Planner}
import Common.Object.SqlParameter
import Common.DBAPI._
import Common.ServiceUtils.schemaName
import cats.effect.IO
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import org.joda.time.DateTime
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
import Utils.TokenProcess.invalidateToken
import cats.implicits.*
import Common.Serialize.CustomColumnTypes.{decodeDateTime,encodeDateTime}

case class UserLogoutMessagePlanner(userToken: String)(override val planContext: PlanContext) extends Planner[String] {
  val logger = LoggerFactory.getLogger(this.getClass.getSimpleName + "_" + planContext.traceID.id)

  override def plan(using planContext: PlanContext): IO[String] = {
    logger.info(s"开始处理用户登出操作，传入的token为：${userToken}")

    for {
      // Step 1: 使用户Token无效化
      _ <- IO(logger.info(s"[Step 1] 调用invalidateToken方法，尝试使Token无效"))
      isInvalidated <- invalidateToken(userToken)

      // 根据无效化结果生成登出消息
      resultMessage <- if (isInvalidated) {
        IO(logger.info(s"[Step 1] Token [${userToken}] 无效化操作成功")) >>
          IO("登出成功！")
      } else {
        IO(logger.info(s"[Step 1] Token [${userToken}] 无效化操作失败")) >>
          IO("登出失败，请检查Token是否有效！")
      }

      _ <- IO(logger.info(s"[结束] 用户Token [${userToken}] 登出结果信息: ${resultMessage}"))
    } yield resultMessage
  }
}