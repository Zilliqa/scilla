# Test Cases
Description of test cases

### Successful Test Cases

| No. |  Category |      Description     |
|-----|---|---| 
|1      |   Mint | Successful add token id 1 |
|2      |   Mint | Successful add token id 2 |                                                       |
|3      |   Transfer (by TokenOwner) | Successful transfer from `0x...92` to `0x...91`| 
|4      |   balanceOf | Check the balance of a particular address |
|5      |   balanceOf | Check the balance of a particular address that does not have token |  
|6      |   ownerOf | Check `ownerOf` tokenID: 1 . To be completed with Events |
|7      |   Approve | `0x...92`approves `0x...91` permission to transfer token ID 1       |
|8      |   ApproveForAll | `0x...92` approves `0x...91`. |                                                           |
|9      |   ApproveForAll | `0x...92` approves `0x...93`. `0x...92` should now have two entries in the approvals.  |
|10      |   Transfer (by operator) | `92` assigns `91` as the operator for his tokens. `91` transfer token ID `2` to himself. |
|11     |   Transfer (with token approval) | `0x...92` tranfer `91`'s token to `93`.   |
|12     |   Delegated Approved | `92` delegates `91` as an approved operator. `91` successfully approves `93` to transfer tokenId 1   |

### Unsuccessful Test Cases
These test cases should fail

| No. |  Category |      Description     |
|-----|---|---|
|21      |   Mint | Unsuccessful as sender is not the token owner. Expected error code: `2` |
|22      |   Mint | Unsuccessful as token has already been minted. Expected error code: `6` |
|23      |   Transfer | Unsuccessful as the sender is not the token owner. Unexpected error code `2`|
|24      |   Transfer | Unsuccessful as the token does not exist. Unexpected error code `4` 
|25      |   Transfer | Unsuccessful as tokenOwner do not match From. Unexpected error code `5` |
|26      |   Approve  | Unsuccessful as the sender is not the token owner                |
|27      |   ApproveForAll  | Unsuccessful as the sender is trying to set himself as operator |
