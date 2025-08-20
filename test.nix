{
  pkgs,
  stlcpp,
}:

pkgs.testers.runNixOSTest {
  name = "stlcpp-test";

  nodes.client =
    { ... }:
    {
      environment.systemPackages = [ stlcpp ];
    };

  testScript = ''
    #import os

    start_all()

    with subtest("std"):
      output = client.succeed("echo 'map' | stlcpp")
      assert ":: forall A, forall B, (A -> B) -> [A] -> [B]" in output

    # with subtest("simple.stlc"):
    #   output = client.succeed("echo 'a' | stlcpp " + "${./examples}/simple.stlc")
    #   assert "5 :: Int" in output
    #   output = client.succeed("echo 'sum' | stlcpp " + "${./examples}/simple.stlc")
    #   assert ":: [Int] -> Int" in output
    #
    # with subtest("list.stlc"):
    #   output = client.succeed("echo 'sum' | stlcpp " + "${./examples}/list.stlc")
    #   assert "[Int] -> Int" in output
    #
    #   output = client.succeed("echo 'append' | stlcpp " + "${./examples}/list.stlc")
    #   assert "forall T, [T] -> [T] -> [T]" in output
    #
    #   output = client.succeed("echo 'reverse' | stlcpp " + "${./examples}/list.stlc")
    #   assert "forall T, [T] -> [T]" in output
    #
    #   output = client.succeed("echo 'map' | stlcpp " + "${./examples}/list.stlc")
    #   assert "forall A, forall B, (A -> B) -> [A] -> [B]" in output
    #
    #   output = client.succeed("echo 'count' | stlcpp " + "${./examples}/list.stlc")
    #   assert "forall A, [A] -> Int" in output
    #
    # with subtest("var.stlc"):
    #   output = client.succeed("echo 'main' | stlcpp " + "${./examples}/var.stlc")
    #   assert "5 :: Int" in output
    #
    # with subtest("fib.stlc"):
    #   output = client.succeed("echo 'fib' | stlcpp " + "${./examples}/fib.stlc")
    #   assert ":: Int -> Int" in output
    #   output = client.succeed("echo 'fibn' | stlcpp " + "${./examples}/fib.stlc")
    #   assert ":: Int -> [Int]" in output
    #   output = client.succeed("echo 'main' | stlcpp " + "${./examples}/fib.stlc")
    #   assert "[55, 34, 21, 13, 8, 5, 3, 2, 1, 1] :: [Int]" in output
    #
    # with subtest("church.stlc"):
    #   output = client.succeed("echo 'id' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: forall T, T -> T" in output
    #   output = client.succeed("echo 'tru' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: forall T, T -> T -> T" in output
    #   output = client.succeed("echo 'fals' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: forall T, T -> T -> T" in output
    #   output = client.succeed("echo 'not' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: (forall T, T -> T -> T) -> forall T, T -> T -> T" in output
    #   output = client.succeed("echo 'unchurchbool' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: (forall T, T -> T -> T) -> Bool" in output
    #   output = client.succeed("echo 'zero' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: forall T, (T -> T) -> T -> T" in output
    #   output = client.succeed("echo 'succ' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: (forall T, (T -> T) -> T -> T) -> forall T, (T -> T) -> T -> T" in output
    #   output = client.succeed("echo 'one' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: forall T, (T -> T) -> T -> T" in output
    #   output = client.succeed("echo 'add' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: (forall T, (T -> T) -> T -> T) -> (forall T, (T -> T) -> T -> T) -> forall T, (T -> T) -> T -> T" in output
    #   output = client.succeed("echo 'unchurchnat' | stlcpp " + "${./examples}/church.stlc")
    #   assert ":: (forall T, (T -> T) -> T -> T) -> Int" in output
    #
    # with subtest("import.stlc"):
    #   output = client.succeed("echo 'sum' | stlcpp " + "${./examples}/import.stlc")
    #   assert "[Int] -> Int" in output
    #
    #   output = client.succeed("echo 'append' | stlcpp " + "${./examples}/import.stlc")
    #   assert "forall T, [T] -> [T] -> [T]" in output
    #
    #   output = client.succeed("echo 'reverse' | stlcpp " + "${./examples}/import.stlc")
    #   assert "forall T, [T] -> [T]" in output
    #
    #   output = client.succeed("echo 'map' | stlcpp " + "${./examples}/import.stlc")
    #   assert "forall A, forall B, (A -> B) -> [A] -> [B]" in output
    #
    #   output = client.succeed("echo 'count' | stlcpp " + "${./examples}/import.stlc")
    #   assert "forall A, [A] -> Int" in output
    #
    #   output = client.succeed("echo 'main' | stlcpp " + "${./examples}/import.stlc")
    #   assert "11 :: Int" in output
    #
    # with subtest("syntax.stlc"):
    #   output = client.succeed("echo 'factfive' | stlcpp " + "${./examples}/syntax.stlc")
    #   assert "120 :: Int" in output
    #   output = client.succeed("echo '6!' | stlcpp " + "${./examples}/syntax.stlc")
    #   assert "720 :: Int" in output

    #for example in os.listdir("${./examples}"):
    #  filename = os.fsdecode(example)
    #  with subtest("Example: " + filename):
    #    client.succeed("echo '42' | stlcpp " + "${./examples}/" + filename)
  '';
}
