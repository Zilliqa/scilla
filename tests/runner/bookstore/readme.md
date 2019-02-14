# Bookstore
Bookstore is an example contract which simulates how a CRUD app might be created in Scilla.

As Scilla is an intermediate-level programming language, it does not support structs construct. Scilla learners might find it difficult to learn Scilla if they can't imagine how to create the most basic application expected - the CRUD application.

For instance, this example shows how a Member object might be created.

```c++
// c++ equivalent
struct Member {
    ByStr20 addr;
    String name;
    Uint32 membershipType;
}
```

The member is then stored in a data structure called `Map`, which indexes a list of members based on the primary key (`addr`).

## Test cases

1. Add member: Owner adds member. Expected: Success.
2. Add member: Non-owner adds member. Expected: `code_not_authorized` (3)
3. Add member: Owner tries to add member but entered a wrong member type. Expected: `code_invalid_params` (4) 
4. Add Book: `_sender` successfully add book
5. Remove Book: `_sender` successfully removes book
6. Update Book: `_sender` successfully updates book 
7. Remove Book: Unsuccessful as book does not exist
8. Update Book: Unsuccessful as book does not exist
9. Add Book: Add book unsuccessful. Book already exist.
10. Add Book: Add second book successful.