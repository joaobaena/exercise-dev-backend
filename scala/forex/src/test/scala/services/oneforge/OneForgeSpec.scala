package services.oneforge

import forex.config.OneForgeConfig
import forex.domain.{Currency, Price, Rate}
import forex.services.oneforge.{OneForgeCache, OneForgeClient, OneForgeError}
import forex.services.oneforge.Protocol.OneForgeRate
import services.oneforge.OneForgeCacheSpec.cacheConfig
import sttp.client4._
import sttp.client4.httpclient.zio._
import sttp.model.StatusCode
import zio._
import zio.test._

import java.time.{Instant, OffsetDateTime, ZoneOffset}

object OneForgeSpec extends ZIOSpecDefault {
  private val testPair = Rate.Pair(Currency.CAD, Currency.SGD)
  private val testRate = OneForgeRate(1.2345, 1.2345, 1.2345, "CAD/SGD", 1690548673301L)

  private val configLayer: ULayer[OneForgeConfig] =
    ZLayer.succeed(OneForgeConfig("https://api.1forge.com", "YOUR_API_KEY"))

  private val cacheLayer = ZLayer.succeed(OneForgeCacheSpec.cacheConfig) ++ TestClock.default >>> OneForgeCache.live

  private def buildClient(sttpClient: SttpClient) =
    (for {
      config <- ZIO.service[OneForgeConfig]
      client <- ZIO.service[SttpClient]
      cache  <- ZIO.service[OneForgeCache]
    } yield OneForgeClient(config, client, cache)).provideLayer(configLayer ++ ZLayer.succeed(sttpClient) ++ cacheLayer)

  def spec =
    suite("OneForgeClient")(
      test("returns the correct rate for a valid pair") {
        val sttpClient: SttpClient = HttpClientZioBackend.stub.whenAnyRequest
          .thenRespond(Response.ok(Right(List(testRate))))
        for {
          client <- buildClient(sttpClient)
          result <- client.get(testPair)
        } yield assertTrue(result == Rate(
          Rate.Pair(testPair.from, testPair.to),
          Price(testRate.p),
          OffsetDateTime.ofInstant(Instant.ofEpochMilli(testRate.t), ZoneOffset.UTC)
        ))
      },
      test("properly returns deserialization errors") {
        val sttpClient: SttpClient = HttpClientZioBackend.stub.whenAnyRequest
          .thenRespond(Response.ok("Invalid Response"))
        for {
          client <- buildClient(sttpClient)
          result <- client.get(testPair).flip
        } yield result match {
          case OneForgeError.Deserialization(body, _) =>
            assertTrue(body.contains("Invalid Response"))
          case _                                      => assertTrue(false)
        }
      },
      test("properly returns http errors") {
        val sttpClient: SttpClient = HttpClientZioBackend.stub.whenAnyRequest
          .thenRespond(Response("Server Error", StatusCode.InternalServerError))
        for {
          client <- buildClient(sttpClient)
          result <- client.get(testPair).flip
        } yield assertTrue(result == OneForgeError.Http("Server Error", 500))

      }
    )

}
