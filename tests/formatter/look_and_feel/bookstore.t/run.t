  $ scilla-fmt bookstore.scilla
  scilla_version 0
  (*
   * Bookstore Contract
   * 
   * This contract demonstrates how a developer can build a CRUD-like smart contract easily.
   * Concepts covered:
   * - Procedures
   * - Custom ADTs
   * - Simple Access Controls
   * 
   * Access control logic is delibrately left out for brevity. You can reference AddMember
   * for how you can implement access control to prevent unauthorised access to transitions.
   *)
  
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  library BookStore
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  (* error codes library *)
  let code_success = Uint32 0
  
  let code_book_not_found = Uint32 1
  
  let code_not_authorized = Uint32 2
  
  let code_invalid_params = Uint32 3
  
  let code_bookid_exist = Uint32 4
  
  let code_store_not_open = Uint32 5
  
  (*
   * Book is an ADT with two fields:
   * Book = { book_title, author }
   *)
  type Book =
  | Book of String String
  
  (*
   * Member is an ADT with two fields:
   * Member = { name , membership_type }
   *)
  type Member =
  | Member of String Uint32
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract BookStore
    (
      owner : ByStr20,
      store_name : String
    )
  
  
  (* storeName can be immutable if it won't ever be changed             *)
  (* Membership data consists of three attributes                       *)
  (* Equivalent member data in C++ struct                               *)
  (* struct Member {                                                    *)
  (*     ByStr20 address;                                               *)
  (*     String name;                                                   *)
  (*     Uint32 membershipType; // 0: Regular, 1: Premium, 2: Corporate *)
  (* }                                                                  *)
  (* Where address is the "key"                                         *)
  field members : Map ByStr20 Member = Emp (ByStr20) (Member)
  
  (* bookstore is opened by default during initialisation *)
  field is_store_open : Bool = True
  
  (* Bookinventory will store a Map of Books                          *)
  field bookInventory : Map Uint32 Book = Emp (Uint32) (Book)
  
  (* Book data consists of three attributes: BookID, Title and Author*)
  (* Equivalent member data in C++ struct *)
  (* struct Book {                                                    *)
  (*     Uint32 BookID;                                               *)
  (*     String Book_title;                                           *)
  (*     String Author;                                               *)
  (* }                                                                *)
  procedure EmitMemberEvent (status : Bool, status_code : Uint32, msg : String)
    match status with
    | True =>
      e = { _eventname : "AddMemberSuccess"; code : status_code; message : msg };
      event e
    | False =>
      e = { _eventname : "AddMemberFailure"; code : status_code; message : msg };
      event e
    end
  end
  
  procedure EmitBookEvent
    (status : Bool, status_code : Uint32, event_action : String, book_id : Uint32)
    match status with
    | True =>
      e =
        {
          _eventname : "BookEventSuccess";
          code : status_code;
          action : event_action;
          id : book_id
        };
      event e
    | False =>
      e =
        {
          _eventname : "BookEventFailure";
          code : status_code;
          action : event_action;
          id : book_id
        };
      event e
    end
  end
  
  (***************************************************)
  (*                 Transitions                     *)
  (***************************************************)
  (*
   * OpenStore: Set the store as open or close
   * @param: is_open { Boolean }
   *)
  transition OpenStore (is_open : Bool)
    (* Access control: Checking if sender is the owner of the Contract *)
    is_authorized = builtin eq _sender owner;
    match is_authorized with
    | True =>
      is_store_open := is_open;
      e = { _eventname : "OpenStore"; status : is_open };
      event e
    | False =>
      (* Unauthorized transaction *)
      status = False;
      msg = "Unauthorised Transaction";
      EmitMemberEvent status code_not_authorized msg
    end
  end
  
  (* @notice: add member is an example. It is not used in other functions. *)
  (* @dev: in real contracts, a developer can use a members mapping to manage *)
  (* access controls to grant a user permission to perform certain actions *)
  (* (e.g. add/remove books) *)
  transition AddMember
    (name : String, member_address : ByStr20, member_type : Uint32)
    (* Access control: Checking if sender is the owner of the Contract *)
    is_authorized = builtin eq _sender owner;
    match is_authorized with
    | True =>
      (* Only the owner can add member *)
      (* Check if membership type is valid. *)
      valid_type = let three = Uint32 3 in builtin lt member_type three;
      match valid_type with
      | True =>
        new_member = Member name member_type;
        members[member_address] := new_member;
        status = True;
        EmitMemberEvent status code_success name
      | False =>
        (* Code for the membership type is invalid *)
        status = False;
        msg = "Invalid membership type";
        EmitMemberEvent status code_invalid_params msg
      end
    | False =>
      (* Unauthorized transaction *)
      status = False;
      msg = "Unauthorised Transaction";
      EmitMemberEvent status code_not_authorized msg
    end
  end
  
  (* @notice: Allows a `_sender` to add a book to the bookstore *)
  (* @dev   : Access controls are omitted for brevity. In production contracts, *)
  (*          you will want to implement proper access controls to allow only *)
  (*          an owner or member to add a book. *)
  transition AddBook (book_title : String, author : String, book_id : Uint32)
    (* @dev: Preconditions can be set to allow only members to add a book *)
    (* @dev: Access controls logic omitted for brevity *)
    is_open <- is_store_open;
    match is_open with
    | True =>
      does_book_exist <- exists bookInventory[book_id];
      match does_book_exist with
      | True =>
        action = "Add";
        status = False;
        EmitBookEvent status code_bookid_exist action book_id
      | False =>
        (* Creating a new Book Model *)
        (* A new book model is a Pair of book_title and author *)
        new_book = Book book_title author;
        (* Add the new book to the book_inventory Map, with BookID as the key *)
        bookInventory[book_id] := new_book;
        action = "Add";
        status = True;
        EmitBookEvent status code_success action book_id
      end
    | False =>
      (* Store is not open *)
      action = "Add";
      status = False;
      EmitBookEvent status code_store_not_open action book_id
    end
  end
  
  (* @notice: Allows a `_sender` to remove a book from the bookstore *)
  (* @dev   : Access controls are omitted for brevity. In production contracts, *)
  (*          you will want to implement proper access controls to allow only *)
  (*          an owner or member to remove a book. *)
  transition RemoveBook (book_id : Uint32)
    (* @dev: Preconditions can be set to allow only members to remove a book *)
    (* @dev: Access controls logic omitted for brevity *)
    is_open <- is_store_open;
    match is_open with
    | True =>
      get_book <- bookInventory[book_id];
      match get_book with
      | Some (Book book_title _) =>
        action = "Remove";
        status = True;
        EmitBookEvent status code_success action book_id;
        delete bookInventory[book_id]
      | None =>
        book_title = "Error: Not Found";
        action = "Remove";
        status = False;
        EmitBookEvent status code_book_not_found action book_id
      end
    | False =>
      (* Store is not open *)
      action = "Add";
      status = False;
      EmitBookEvent status code_store_not_open action book_id
    end
  end
  
  (* @notice: Allows a `_sender` to update a book from the bookstore *)
  (* @dev   : Access controls are omitted for brevity. In production contracts, *)
  (*          you will want to implement proper access controls to allow only *)
  (*          an owner or member to remove a book. *)
  transition UpdateBook (book_id : Uint32, book_title : String, author : String)
    (* @dev: Preconditions can be set to allow only members to update a book *)
    (* @dev: Access controls omitted for brevity *)
    (* preconditions  *)
    does_book_exist <- exists bookInventory[book_id];
    match does_book_exist with
    | False =>
      (* Book ID is not found in the records. *)
      action = "Update";
      status = False;
      EmitBookEvent status code_book_not_found action book_id
    | True =>
      (* constructs book model  *)
      (* Creating a new Book Model *)
      (* A new book model is a Pair of book_title and author *)
      new_book = Book book_title author;
      (* Add the new book to the book_inventory Map, with BookID as the key *)
      bookInventory[book_id] := new_book;
      action = "Update";
      status = True;
      EmitBookEvent status code_success action book_id
    end
  end
  
