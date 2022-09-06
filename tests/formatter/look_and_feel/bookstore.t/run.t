  $ scilla-fmt bookstore.scilla
  scilla_version 0
  
  library BookStore
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let code_success = Uint32 0
  
  let code_book_not_found = Uint32 1
  
  let code_not_authorized = Uint32 2
  
  let code_invalid_params = Uint32 3
  
  let code_bookid_exist = Uint32 4
  
  let code_store_not_open = Uint32 5
  
  type Book =
  | Book of String String
  
  type Member =
  | Member of String Uint32
  
  
  contract BookStore
    (
      owner : ByStr20,
      store_name : String
    )
  
  
  field members : Map ByStr20 Member = Emp (ByStr20) (Member)
  
  field is_store_open : Bool = True
  
  field bookInventory : Map Uint32 Book = Emp (Uint32) (Book)
  
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
  
  transition OpenStore (is_open : Bool)
    is_authorized = builtin eq _sender owner;
    match is_authorized with
    | True =>
      is_store_open := is_open;
      e = { _eventname : "OpenStore"; status : is_open };
      event e
    | False =>
      status = False;
      msg = "Unauthorised Transaction";
      EmitMemberEvent status code_not_authorized msg
    end
  end
  
  transition AddMember
    (name : String, member_address : ByStr20, member_type : Uint32)
    is_authorized = builtin eq _sender owner;
    match is_authorized with
    | True =>
      valid_type = let three = Uint32 3 in builtin lt member_type three;
      match valid_type with
      | True =>
        new_member = Member name member_type;
        members[member_address] := new_member;
        status = True;
        EmitMemberEvent status code_success name
      | False =>
        status = False;
        msg = "Invalid membership type";
        EmitMemberEvent status code_invalid_params msg
      end
    | False =>
      status = False;
      msg = "Unauthorised Transaction";
      EmitMemberEvent status code_not_authorized msg
    end
  end
  
  transition AddBook (book_title : String, author : String, book_id : Uint32)
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
        new_book = Book book_title author;
        bookInventory[book_id] := new_book;
        action = "Add";
        status = True;
        EmitBookEvent status code_success action book_id
      end
    | False =>
      action = "Add";
      status = False;
      EmitBookEvent status code_store_not_open action book_id
    end
  end
  
  transition RemoveBook (book_id : Uint32)
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
      action = "Add";
      status = False;
      EmitBookEvent status code_store_not_open action book_id
    end
  end
  
  transition UpdateBook (book_id : Uint32, book_title : String, author : String)
    does_book_exist <- exists bookInventory[book_id];
    match does_book_exist with
    | False =>
      action = "Update";
      status = False;
      EmitBookEvent status code_book_not_found action book_id
    | True =>
      new_book = Book book_title author;
      bookInventory[book_id] := new_book;
      action = "Update";
      status = True;
      EmitBookEvent status code_success action book_id
    end
  end
  
