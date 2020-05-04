@0xb13fc2f2a4c1d65b;

interface Callback {
  log @0 (msg :Text) -> ();
}

interface Echo {
  ping @0 (msg :Text) -> (reply :Text);
  heartbeat @1 (msg :Text, callback :Callback) -> ();
}
