object Exercise7_Test extends App:
  import FirstStepsWithZIO.Exercise7.*

  assert(collectAll(List(succeed(1), succeed(2), succeed(3))).run(()) == Right(List(1, 2, 3)))
  assert(collectAllV2(List(succeed(1), succeed(2), succeed(3))).run(()) == Right(List(1, 2, 3)))
