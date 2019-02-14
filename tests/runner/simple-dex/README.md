# Simple DEX
Simple Decentralized Exchange contract to allow P2P trading of fungible-token contracts.

## Disclaimer
This contract is built for testing. Do not use this contract in production

## Features
* Make order
* Fill order
* Claim back
* Cancel order


## Sequence of events
1. Maker makes an order through `makeOrder`. This will invoke a `transferFrom` transition in tokenA's fungible contract to transfer tokens from maker's address to dex contract address
2. Taker fills an order using `fillOrder(orderId)`. This will invoke a `transferFrom` transition in tokenB's fungible contract to transfer tokens from taker's address to DEX contract address
3. Maker and Taker can claim back if there are `pendingReturns`. To claim back tokens, maker/taker can call the `ClaimBack(tokenContractAddress)`. This will invoke the `transferFrom` transition in token's fungible contract to transfer tokens from DEX contract address to `_sender`.

## Test Cases

| Number | Description | Expected Result |
|---|---|---|
|1| Maker makes an order to trade valueA of tokenA for valueB of tokenB | Success |
|2| Taker fills order | Success |
|3| Another taker fills order of the same ID. | Failure |
|4| Maker claims back the funds. Contract calls another smart contract to transfer tokens to maker. | Success |
|5| Taker claims back the funds. Contract calls another smart contract to transfer tokens to taker. | Success |
|6| Taker tries to reclaim the funds | Failure |
|7| Maker cancels order | Success |
|8| Sender (who is not the maker) tries to cancel order | Failure |

