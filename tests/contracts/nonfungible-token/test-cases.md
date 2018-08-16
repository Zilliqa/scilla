# Test Cases
Description of test cases

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
|9      |   ApproveForAll | `0x...92` approves `0x...93`. `0x...92` should now have two entries in the approvals.  |
|11     |   Transfer (with token approval) | `0x...92` tranfer `91`'s token to `93`.   |
|12     |   Delegated Approved | `92` delegates `91` as an approved operator. `91` successfully approves `93` to transfer tokenId 1   |