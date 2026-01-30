import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.time.Instant
import java.util.concurrent.TimeUnit

/**   1. Write a ZIO program that simulates a countdown timer (e.g., prints numbers from 5 to 1, with a 1-second delay
  * between each). Test this program using TestClock.
  */
//noinspection SimplifyUnlessInspection
object CountdownTimer extends ZIOSpecDefault:
  def countdown(n: Int): ZIO[Any, Nothing, Unit] =
    if n <= 0 then ZIO.unit
    else
      for
        _ <- Console.printLine(n).orDie
        _ <- ZIO.sleep(1.second)
        _ <- countdown(n - 1)
      yield ()

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Countdown Timer Spec")(
      test("should count down from 5 to 1") {
        for
          fiber         <- countdown(5).fork
          _             <- TestClock.adjust(5.seconds)
          _             <- fiber.join
          consoleOutput <- TestConsole.output
        yield assertTrue(
          consoleOutput == Vector(
            "5\n",
            "4\n",
            "3\n",
            "2\n",
            "1\n"
          )
        )
      }
    )

/** 2. Create a simple cache that expires entries after a certain duration. Implement a program that adds items to the
  * cache and tries to retrieve them. Write tests using `TestClock` to verify that items are available before expiration
  * and unavailable after expiration.
  */
//noinspection ScalaWeakerAccess, TypeAnnotation, SimplifyAssertInspection
object CacheWithExpiration extends ZIOSpecDefault:

  case class CacheEntry[V](value: V, expirationTime: Long)

  class Cache[K, V](storageMapRef: Ref[Map[K, CacheEntry[V]]], expiration: Long):

    def printLine(s: => String): ZIO[Any, Nothing, Unit] = Console.printLine(s).orDie

    def put(key: K, value: V): ZIO[Any, Nothing, Unit] =
      for
        currentTime <- Clock.currentTime(TimeUnit.MILLISECONDS)

        expirationTime = currentTime + expiration

        _ <- printLine(
          s"Putting value=$value by key=$key into cache; " +
            s"currentTime=$currentTime, expirationTime=$expirationTime millis}"
        )

        _ <- storageMapRef.update(_ + (key -> CacheEntry(value, expirationTime)))

        storageMap <- storageMapRef.get

        _ <- printLine(
          s"Put value=$value by key=$key to cache. " +
            s"Current cache state: $storageMap"
        )
      yield ()

    def get(key: K): ZIO[Any, Nothing, Option[V]] =
      for
        currentTime <- Clock.currentTime(TimeUnit.MILLISECONDS)

        storageMap <- storageMapRef.get

        _ <- printLine(
          s"Getting value from cache by key=$key. " +
            s"Current cache state: $storageMap; currentTime=$currentTime"
        )

        result = storageMap.get(key) match
          case Some(entry) if entry.expirationTime > currentTime =>
            Some(entry.value)
          case _ =>
            None

        _ <- printLine(s"Got value from cache: $result by key=$key")

        _ <- storageMapRef.update(_.filter { case (_, entry) => entry.expirationTime > currentTime })

        cleanedUpStorageMap <- storageMapRef.get

        _ <- printLine(
          s"Cleaned up expired values. " +
            s"Cache state after cleanup: $cleanedUpStorageMap"
        )
      yield result

  object Cache:
    def make[K, V](expiration: Duration): ZIO[Any, Nothing, Cache[K, V]] =
      Ref.make(Map.empty[K, CacheEntry[V]]).map(new Cache[K, V](_, expiration.toMillis))

  override def spec =
    val key   = "foo"
    val value = 42

    suite("Cache With Expiration Spec")(
      test("should store and retrieve items before expiration") {
        for
          cache     <- Cache.make[String, Int](1.minute)
          _         <- cache.put(key, value)
          _         <- TestClock.adjust(10.seconds)
          readValue <- cache.get(key)
        yield assert(readValue)(isSome(equalTo(value)))
      },
      test("should not retrieve items after expiration") {
        for
          cache <- Cache.make[String, Int](1.minute)
          _     <- cache.put(key, value)
          _     <- TestClock.adjust(65.seconds)
          value <- cache.get(key)
        yield assert(value)(isNone)
      },
      test("should handle multiple items with different expiration times") {
        for
          cache  <- Cache.make[String, Int](2.minutes)      // 2 minutes expiration
          _      <- cache.put("foo", 42)
          _      <- TestClock.adjust(1.minute)              // 1 minute passed
          _      <- cache.put("bar", 21)                    // This will expire 1 minute after foo
          _      <- TestClock.adjust(1.minute + 30.seconds) // Total 2 minutes 30 seconds passed
          value1 <- cache.get("foo")                        // Should be expired
          value2 <- cache.get("bar")                        // Should still be valid
        yield assert(value1)(isNone) && assert(value2)(isSome(equalTo(21)))
      },
      test("should overwrite existing keys with new expiration time") {
        for
          cache <- Cache.make[String, Int](1.minute) // 1 minute expiration
          _     <- cache.put("foo", 42)
          _     <- TestClock.adjust(59.seconds)      // Almost expired
          _     <- cache.put("foo", 21)              // Reset expiration
          _ <-
            TestClock.adjust(31.seconds) // Total 1 minute 30 seconds from first put
          value <- cache.get("foo")
        yield assert(value)(isSome(equalTo(21)))
      }
    )

/** 3. Create a rate limiter that allows a maximum of N operations per minute. Implement a program that uses this rate
  * limiter. Write tests using `TestClock` to verify that the rate limiter correctly allows or blocks operations based
  * on the time window.
  */
//noinspection SimplifyAssertInspection,TypeAnnotation,ScalaWeakerAccess
object RateLimiterSpec extends ZIOSpecDefault:

  class RateLimiter(operationTimestampsRef: Ref[List[Instant]], maxOps: Int):
    def tryAcquire: UIO[Boolean] =
      for
        now <- Clock.instant

        oneMinuteAgo = now.minusSeconds(60)

        acquired <- operationTimestampsRef.modify { timestamps =>
          // Remove timestamps older than 1 minute
          val validTimestamps = timestamps.filter(_.isAfter(oneMinuteAgo))

          // Check if we can add a new operation
          if validTimestamps.size < maxOps then
            // Add current timestamp and allow operation
            (true, now :: validTimestamps)
          else
            // Rate limit exceeded
            (false, validTimestamps)
        }
      yield acquired

  object RateLimiter:
    def make(maxOps: Int): UIO[RateLimiter] =
      for operationTimestampsRef <- Ref.make[List[Instant]](List.empty)
      yield new RateLimiter(operationTimestampsRef, maxOps)

  override def spec =
    suite("Rate Limiter Spec")(
      test("should allow operations within rate limit") {
        for
          rateLimiter <- RateLimiter.make(5) // Allow 5 ops per minute

          // Try 5 operations - all should be allowed
          results <- ZIO.foreach(1 to 5)(_ => rateLimiter.tryAcquire)
        yield assert(results)(forall(equalTo(true)))
      },
      test("should block operations exceeding rate limit") {
        for
          rateLimiter <- RateLimiter.make(3) // Allow 3 ops per minute

          // First 3 operations should be allowed
          firstBatch <- ZIO.foreach(1 to 3)(_ => rateLimiter.tryAcquire)

          // Next 2 operations should be blocked
          secondBatch <- ZIO.foreach(1 to 2)(_ => rateLimiter.tryAcquire)
        yield assert(firstBatch)(forall(equalTo(true))) &&
          assert(secondBatch)(forall(equalTo(false)))
      },
      test("should reset after time window passes") {
        for
          rateLimiter <- RateLimiter.make(2) // Allow 2 ops per minute

          // Use up the limit
          _ <- rateLimiter.tryAcquire
          _ <- rateLimiter.tryAcquire

          // This should be blocked
          blockedResult <- rateLimiter.tryAcquire

          // Advance time by 61 seconds
          _ <- TestClock.adjust(61.seconds)

          // Now operations should be allowed again
          allowedResult1 <- rateLimiter.tryAcquire
          allowedResult2 <- rateLimiter.tryAcquire
        yield assert(blockedResult)(equalTo(false)) &&
          assert(allowedResult1)(equalTo(true)) &&
          assert(allowedResult2)(equalTo(true))
      },
      test("should use sliding window for rate limiting") {
        for
          rateLimiter <- RateLimiter.make(3) // Allow 3 ops per minute

          // Perform operations at different times
          op1 <- rateLimiter.tryAcquire // t=0
          _   <- TestClock.adjust(20.seconds)

          op2 <- rateLimiter.tryAcquire // t=20
          _   <- TestClock.adjust(20.seconds)

          op3 <- rateLimiter.tryAcquire // t=40

          // Should be blocked (3 ops in last 60 seconds)
          op4 <- rateLimiter.tryAcquire // t=40

          _ <- TestClock.adjust(21.seconds) // t=61

          // First operation is now outside the window, so this should be allowed
          op5 <- rateLimiter.tryAcquire // t=61
        yield assert(op1)(equalTo(true)) &&
          assert(op2)(equalTo(true)) &&
          assert(op3)(equalTo(true)) &&
          assert(op4)(equalTo(false)) &&
          assert(op5)(equalTo(true))
      },
      test("should handle burst of operations correctly") {
        for
          rateLimiter <- RateLimiter.make(5) // Allow 5 ops per minute

          // Burst of 10 operations at once
          results <- ZIO.foreach(1 to 10)(_ => rateLimiter.tryAcquire)

          allowed = results.take(5)
          blocked = results.drop(5)
        yield assert(allowed)(forall(equalTo(true))) &&
          assert(blocked)(forall(equalTo(false)))
      }
    )
end RateLimiterSpec

/** 4. Implement a function that reverses a list, then write a property-based test to verify that reversing a list twice
  * returns the original list.
  */
object ReverseListSpec extends ZIOSpecDefault:
  def reverseList[A](list: List[A]): List[A] = ???

  override def spec =
    suite("Reverse List Spec")(
      test("reversing a list twice returns the original list") {
        ???
      }
    )

/** 5. Implement an AVL tree (self-balancing binary search tree) with insert and delete operations. Write property-based
  * tests to verify that the tree remains balanced after each operation. A balanced tree is one where the height of
  * every node's left and right subtrees differs by at most one.
  */
object AVLTreeSpec extends ZIOSpecDefault:

  // A simple AVL Tree implementation here

  override def spec =
    suite("AVL Tree Spec")(
      suite("Balance Properties")(
        test("should maintain balance after insertions") {
          ???
        },
        test("should maintain balance after deletions") {
          ???
        }
      )
    )
