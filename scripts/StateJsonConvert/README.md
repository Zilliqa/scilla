# Convert State JSON from Zilliqa API Format to Scilla Format

The [Zilliqa API]((https://dev.zilliqa.com/docs/apis/api-contract-get-smartcontract-state))
to fetch the state of a contract produces a JSON in the format

```
{
  "_balance": "0",
  "admins": {
    "0xdfa89866ae86632b36361d53b76c1373448c28fa": {
      "argtypes": [],
      "arguments": [],
      "constructor": "True"
    }
  }
}
```

This program converts it into a format that Scilla understands. Since
there is some loss of information in the API output (the type of a field,
for example), this program also requires the `-contractinfo` output of
scilla-checker as an additional input to find this missing information.
The init JSON of the contract must be provided as input to `scilla-checker`
(via the `-init` option) so that names are disambiguated correctly in the
output contract info JSON of `scilla-checker`.

The output JSON for the above snippet would look something like this

```
[
  {
    "vname" : "_balance"
    "type" : "Uint128",
    "value" : "0"
  },
  {
    "vname" : "admins",
    "type" : "Map ByStr20 Bool",
    "value" : [
      {
        "key" : "0xdfa89866ae86632b36361d53b76c1373448c28fa",
        "val" : {
          "argtypes": [],
          "arguments": [],
          "constructor": "True"
        }
      }
    ]
  }
]
```

## Build
Install `libjsoncpp-dev`and `libboost-program-options-dev` system packages.
The command `make` in this directory should now succeed in building the
executable `convert.exe`.

## Use
Using the example inputs provided in `tests`, here's how the tool can be used:

```./convert.exe -s tests/ZilSwap/state.json -c tests/ZilSwap/contractinfo.json```
