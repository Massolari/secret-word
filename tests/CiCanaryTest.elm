module CiCanaryTest exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "falha de propósito para verificar a CI" <|
        \_ -> Expect.fail "teste temporário"
