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
      output = client.succeed("echo 'map' | stlcpp")

    for example in os.listdir("${./examples}"):
      filename = os.fsdecode(example)
      if not filename.endswith(".stlc"):
        continue
      with subtest("Example: " + filename):
        client.succeed("echo '42' | stlcpp " + "${./examples}/" + filename)
  '';
}
