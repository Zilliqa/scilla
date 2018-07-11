Test Cases

1. Get total supply: Returns total supply
2. Get Name: Returns name of NFT
3. Get symbol: returns name of symbol
4. Transfer: When `_sender` is the `owner` of the contract. Proceeds to mine new token and assign it to `_to`.
5. Transfer: P2P transfer - previous owner sends the token to recipient. 
6. Unauthorized transfer: When `_sender` is not the current owner. System should return invalid code
7. OwnerOf: Token that is issued.
8. Ownerof: Token that is not issued. No owner found.
9. Approval: Approve granted
10. Approval failed: Owner tries to approve himself.
11. Approval failed: `_sender` is not the current owner of the token
12: Transfer failed: `_sender` is not the owner of the token. `_sender` is also not approved by the token owner. This test case differs from case 6; in case 6. the approval map is empty.
13: Transfer Succeeds: `_sender` is not the owner, but he is authorized to make the transaction. 
14: TakeOwnership: `_sender` successfully takes ownership of the token.
15: TakeOwnership: `_sender` is not authorized to take ownership.
