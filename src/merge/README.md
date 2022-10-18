# Merge library

Merge library merges multiple contracts to a single merge contract.

## Configuration

Sometimes we can get names collisions when merging multiple contracts. In that case it's recommended to use the configuration file in the following format:

```json
{
  "replacements": [
    {
      "filename": "tests/merge/static/remote_collisions11.scilla",
      "line": 19,
      "replacee": "m_f",
      "replacement": "remoteCollisions12_m_f"
    },
    {
      "filename": "tests/merge/static/remote_collisions11.scilla",
      "line": 17,
      "replacee": "a_f",
      "replacement": "remoteCollisions13_a_f"
    }
  ]
}
```

It you're using the `scilla-merger` tool, you could pass the file with the given format using the `--config` argument.
