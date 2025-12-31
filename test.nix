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
    import os

    start_all()

    with subtest("std"):
      client.succeed("echo 'map' | stlcpp")
      client.succeed("stlcpp --eval map")

    for example in os.listdir("${./examples}"):
      filename = os.fsdecode(example)
      if not filename.endswith(".stlc"):
        continue
      with subtest("Example: " + filename):
        client.succeed("echo '42' | stlcpp ${./examples}/" + filename)

    with subtest("io"):
      output = client.succeed("echo 'IO' | stlcpp --exec ${./examples}/io.stlc")
      assert "Hello IO!" in output

    with subtest("rng"):
      output = client.succeed("echo \":exec rng\n123\" | stlcpp ${./examples}/io.stlc")
      assert "4759720417705502395" in output
  '';
}
