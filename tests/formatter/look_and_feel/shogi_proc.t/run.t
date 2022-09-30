  $ scilla-fmt shogi_proc.scilla
  scilla_version 0
  
  import
    ShogiLib
    BoolUtils
    ListUtils
  
  library Shogi
  
  let move_east =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin add column one in Square row new_column
      end
  
  let move_southeast =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin add column one in
        let new_row = builtin sub row one in
        Square new_row new_column
      end
  
  let move_south =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_row = builtin sub row one in Square new_row column
      end
  
  let move_southwest =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin sub column one in
        let new_row = builtin sub row one in
        Square new_row new_column
      end
  
  let move_west =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin sub column one in Square row new_column
      end
  
  let move_northwest =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin sub column one in
        let new_row = builtin add row one in
        Square new_row new_column
      end
  
  let move_north =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_row = builtin add row one in Square new_row column
      end
  
  let move_northeast =
    fun (square : Square) =>
      let one = Uint32 1 in
      match square with
      | Square row column =>
        let new_column = builtin add column one in
        let new_row = builtin add row one in
        Square new_row new_column
      end
  
  let move_one_square =
    fun (square : Square) =>
      fun (direction : Direction) =>
        match direction with
        | East => move_east square
        | SouthEast => move_southeast square
        | South => move_south square
        | SouthWest => move_southwest square
        | West => move_west square
        | NorthWest => move_northwest square
        | North => move_north square
        | NorthEast => move_northeast square
        end
  
  let generate_path =
    fun (origin : Square) =>
      fun (direction : Direction) =>
        fun (distance : Uint32) =>
          let distance_as_nat = builtin to_nat distance in
          let init = Nil {(Square)} in
          let folder = @nat_fold (List Square) in
          let f =
            fun (acc : List Square) =>
              fun (count : Nat) =>
                let last_square =
                  match acc with
                  | Cons s _ => s
                  | Nil => origin
                  end
                in
                let off_the_board =
                  match last_square with
                  | Square row column =>
                    let zero = Uint32 0 in
                    let ten = Uint32 10 in
                    let too_far_east = builtin eq zero column in
                    let too_far_west = builtin eq ten column in
                    let too_far_south = builtin eq zero row in
                    let too_far_north = builtin eq ten row in
                    let colum_fail = orb too_far_east too_far_west in
                    let row_fail = orb too_far_north too_far_south in
                    orb colum_fail row_fail
                  end
                in
                match off_the_board with
                | True => acc
                | False =>
                  let next_square = move_one_square last_square direction in
                  Cons {(Square)} next_square acc
                end
          in
          folder f init distance_as_nat
  
  let king_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        let one = Uint32 1 in
        generate_path square direction one
  
  let gold_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (player_1_in_turn : Bool) =>
          match player_1_in_turn with
          | True =>
            match direction with
            | SouthWest => Nil {(Square)}
            | SouthEast => Nil {(Square)}
            | _ => let one = Uint32 1 in generate_path square direction one
            end
          | False =>
            match direction with
            | NorthWest => Nil {(Square)}
            | NorthEast => Nil {(Square)}
            | _ => let one = Uint32 1 in generate_path square direction one
            end
          end
  
  let silver_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (player_1_in_turn : Bool) =>
          match player_1_in_turn with
          | True =>
            match direction with
            | South => Nil {(Square)}
            | East => Nil {(Square)}
            | West => Nil {(Square)}
            | _ => let one = Uint32 1 in generate_path square direction one
            end
          | False =>
            match direction with
            | North => Nil {(Square)}
            | East => Nil {(Square)}
            | West => Nil {(Square)}
            | _ => let one = Uint32 1 in generate_path square direction one
            end
          end
  
  let knight_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (player_1_in_turn : Bool) =>
          match player_1_in_turn with
          | True =>
            let north = North in
            match direction with
            | NorthEast =>
              let nil_path = Nil {(Square)} in
              let first_square = move_one_square square north in
              let final_square = move_one_square first_square direction in
              Cons {(Square)} final_square nil_path
            | NorthWest =>
              let nil_path = Nil {(Square)} in
              let first_square = move_one_square square north in
              let final_square = move_one_square first_square direction in
              Cons {(Square)} final_square nil_path
            | _ => Nil {(Square)}
            end
          | False =>
            let south = South in
            match direction with
            | SouthEast =>
              let nil_path = Nil {(Square)} in
              let first_square = move_one_square square south in
              let final_square = move_one_square first_square direction in
              Cons {(Square)} final_square nil_path
            | SouthWest =>
              let nil_path = Nil {(Square)} in
              let first_square = move_one_square square south in
              let final_square = move_one_square first_square direction in
              Cons {(Square)} final_square nil_path
            | _ => Nil {(Square)}
            end
          end
  
  let pawn_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (player_1_in_turn : Bool) =>
          match player_1_in_turn with
          | True =>
            match direction with
            | North => let one = Uint32 1 in generate_path square direction one
            | _ => Nil {(Square)}
            end
          | False =>
            match direction with
            | South => let one = Uint32 1 in generate_path square direction one
            | _ => Nil {(Square)}
            end
          end
  
  let lance_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (distance : Uint32) =>
          fun (player_1_in_turn : Bool) =>
            match player_1_in_turn with
            | True =>
              match direction with
              | North => generate_path square direction distance
              | _ => Nil {(Square)}
              end
            | False =>
              match direction with
              | South => generate_path square direction distance
              | _ => Nil {(Square)}
              end
            end
  
  let officer_path =
    fun (square : Square) =>
      fun (direction : Direction) =>
        fun (distance : Uint32) =>
          generate_path square direction distance
  
  let movement_path =
    fun (square : Square) =>
      fun (piece : Piece) =>
        fun (promotion_status : PromotionStatus) =>
          fun (direction : Direction) =>
            fun (distance : Uint32) =>
              fun (player_1_in_turn : Bool) =>
                match piece with
                | King =>
                  let one = Uint32 1 in
                  let distance_is_one = builtin eq one distance in
                  match distance_is_one with
                  | True => king_path square direction
                  | False => Nil {(Square)}
                  end
                | GoldGeneral =>
                  let one = Uint32 1 in
                  let distance_is_one = builtin eq one distance in
                  match distance_is_one with
                  | True => gold_path square direction player_1_in_turn
                  | False => Nil {(Square)}
                  end
                | SilverGeneral =>
                  let one = Uint32 1 in
                  let distance_is_one = builtin eq one distance in
                  match distance_is_one with
                  | True =>
                    match promotion_status with
                    | Promoted => gold_path square direction player_1_in_turn
                    | NotPromoted => silver_path square direction player_1_in_turn
                    end
                  | False => Nil {(Square)}
                  end
                | Knight =>
                  match promotion_status with
                  | Promoted =>
                    let one = Uint32 1 in
                    let distance_is_one = builtin eq one distance in
                    match distance_is_one with
                    | True => gold_path square direction player_1_in_turn
                    | False => Nil {(Square)}
                    end
                  | NotPromoted =>
                    let two = Uint32 2 in
                    let distance_is_two = builtin eq two distance in
                    match distance_is_two with
                    | True => knight_path square direction player_1_in_turn
                    | False => Nil {(Square)}
                    end
                  end
                | Pawn =>
                  let one = Uint32 1 in
                  let distance_is_one = builtin eq one distance in
                  match distance_is_one with
                  | True =>
                    match promotion_status with
                    | Promoted => gold_path square direction player_1_in_turn
                    | NotPromoted => pawn_path square direction player_1_in_turn
                    end
                  | False => Nil {(Square)}
                  end
                | Lance =>
                  match promotion_status with
                  | Promoted =>
                    let one = Uint32 1 in
                    let distance_is_one = builtin eq one distance in
                    match distance_is_one with
                    | True => gold_path square direction player_1_in_turn
                    | False => Nil {(Square)}
                    end
                  | NotPromoted =>
                    let zero = Uint32 0 in
                    let distance_is_greater_that_zero = builtin lt zero distance
                    in
                    match distance_is_greater_that_zero with
                    | True =>
                      lance_path square direction distance player_1_in_turn
                    | False => Nil {(Square)}
                    end
                  end
                | Bishop =>
                  match direction with
                  | SouthEast => officer_path square direction distance
                  | SouthWest => officer_path square direction distance
                  | NorthEast => officer_path square direction distance
                  | NorthWest => officer_path square direction distance
                  | _ =>
                    match promotion_status with
                    | NotPromoted => Nil {(Square)}
                    | Promoted =>
                      let one = Uint32 1 in
                      let distance_is_one = builtin eq one distance in
                      match distance_is_one with
                      | True => officer_path square direction one
                      | False => Nil {(Square)}
                      end
                    end
                  end
                | Rook =>
                  match direction with
                  | South => officer_path square direction distance
                  | West => officer_path square direction distance
                  | East => officer_path square direction distance
                  | North => officer_path square direction distance
                  | _ =>
                    match promotion_status with
                    | NotPromoted => Nil {(Square)}
                    | Promoted =>
                      let one = Uint32 1 in
                      let distance_is_one = builtin eq one distance in
                      match distance_is_one with
                      | True => officer_path square direction one
                      | False => Nil {(Square)}
                      end
                    end
                  end
                end
  
  let perform_promotion =
    fun (promote : Bool) =>
      fun (promotion_status : PromotionStatus) =>
        fun (origin_row : Uint32) =>
          fun (target_row : Uint32) =>
            fun (player_1_in_turn : Bool) =>
              match promote with
              | False => Some {(PromotionStatus)} promotion_status
              | True =>
                match promotion_status with
                | Promoted => None {(PromotionStatus)}
                | NotPromoted =>
                  match player_1_in_turn with
                  | True =>
                    let seven = Uint32 7 in
                    let origin_not_in_opponent_territory =
                      builtin lt origin_row seven
                    in
                    let target_not_in_opponent_territory =
                      builtin lt target_row seven
                    in
                    let not_in_opponent_territory =
                      andb
                        origin_not_in_opponent_territory target_not_in_opponent_territory
                    in
                    match not_in_opponent_territory with
                    | True => None {(PromotionStatus)}
                    | False => Some {(PromotionStatus)} promoted
                    end
                  | False =>
                    let three = Uint32 3 in
                    let origin_not_in_opponent_territory =
                      builtin lt three origin_row
                    in
                    let target_not_in_opponent_territory =
                      builtin lt three target_row
                    in
                    let not_in_opponent_territory =
                      andb
                        origin_not_in_opponent_territory target_not_in_opponent_territory
                    in
                    match not_in_opponent_territory with
                    | True => None {(PromotionStatus)}
                    | False => Some {(PromotionStatus)} promoted
                    end
                  end
                end
              end
  
  type Error =
  | GameOver
  | PlayingOutOfTurn
  | IllegalAction
  | InternalError
  
  let game_is_over = GameOver
  
  let playing_out_of_turn = PlayingOutOfTurn
  
  let illegal_action = IllegalAction
  
  let internal_error = InternalError
  
  let mk_error_event =
    fun (err : Error) =>
      let err_code =
        match err with
        | GameOver => Int32 -1
        | PlayingOutOfTurn => Int32 -2
        | IllegalAction => Int32 -3
        | InternalError => Int32 -999
        end
      in
      { _eventname : "ShogiError"; err_code : err_code }
  
  let mk_winner_event =
    fun (current_player : ByStr20) =>
      fun (player1 : ByStr20) =>
        fun (player2 : ByStr20) =>
          let current_player_is_one = builtin eq player1 current_player in
          let player_no =
            match current_player_is_one with
            | True => Uint32 1
            | False => Uint32 2
            end
          in
          { _eventname : "ShogiWinner"; winner : player_no }
  
  let get_next_player =
    fun (current_player : ByStr20) =>
      fun (player1 : ByStr20) =>
        fun (player2 : ByStr20) =>
          let current_player_is_one = builtin eq player1 current_player in
          match current_player_is_one with
          | True => player2
          | False => player1
          end
  
  
  contract ShogiProc
    (
      player1 : ByStr20,
      player2 : ByStr20
    )
  
  
  field board : Map Uint32 (Map Uint32 SquareContents) =
    initial_board player1 player2
  
  field captured_pieces : Map ByStr20 (Map Uint32 Uint32) =
    init_captured_pieces player1 player2
  
  field player_in_turn : Option ByStr20 = Some {(ByStr20)} player1
  
  field winner : Option ByStr20 = None {(ByStr20)}
  
  procedure InternalErrorEvent ()
    none_player = None {(ByStr20)};
    player_in_turn := none_player;
    err = internal_error;
    e = mk_error_event err;
    event e
  end
  
  procedure IllegalActionEvent ()
    err = illegal_action;
    e = mk_error_event err;
    event e
  end
  
  procedure Winner (winning_player : ByStr20)
    none = None {(ByStr20)};
    player_in_turn := none;
    some_winner = Some {(ByStr20)} winning_player;
    winner := some_winner
  end
  
  procedure Resign (current_player : ByStr20)
    opponent = get_next_player current_player player1 player2;
    Winner opponent
  end
  
  procedure PlacePiece (piece : Piece, square : Square)
    piece_no = piece_to_int piece;
    capture_count <- captured_pieces[_sender][piece_no];
    match capture_count with
    | None => InternalErrorEvent
    | Some count =>
      zero = Uint32 0;
      has_pieces = builtin lt zero count;
      match has_pieces with
      | False => IllegalActionEvent
      | True =>
        match square with
        | Square row column =>
          target_square_content <- board[row][column];
          match target_square_content with
          | Some Free =>
            one = Uint32 1;
            new_count = builtin sub count one;
            captured_pieces[_sender][piece_no] := new_count;
            new_contents = Occupied piece not_promoted _sender;
            board[row][column] := new_contents
          | _ => IllegalActionEvent
          end
        end
      end
    end
  end
  
  procedure PerformMoveAndPromote
    (
      current_player : ByStr20,
      piece : Piece,
      promote : Bool,
      promotion_status : PromotionStatus,
      row : Uint32,
      column : Uint32,
      target_row : Uint32,
      target_column : Uint32,
      player_1_moves : Bool
    )
    new_promotion_status_opt =
      perform_promotion promote promotion_status row target_row player_1_moves;
    match new_promotion_status_opt with
    | None => IllegalActionEvent
    | Some new_promotion_status =>
      board[row][column] := free;
      new_contents_at_target = Occupied piece new_promotion_status current_player;
      board[target_row][target_column] := new_contents_at_target
    end
  end
  
  procedure MovePiece
    (
      current_player : ByStr20,
      square : Square,
      direction : Direction,
      distance : Uint32,
      promote : Bool
    )
    match square with
    | Square row column =>
      contents <- board[row][column];
      match contents with
      | Some (Occupied piece promotion_status owner) =>
        correct_owner = builtin eq owner current_player;
        match correct_owner with
        | False => IllegalActionEvent
        | True =>
          player_1_moves = builtin eq current_player player1;
          path =
            movement_path
              square piece promotion_status direction distance player_1_moves;
          match path with
          | Nil => IllegalActionEvent
          | Cons (Square target_row target_column) intervening_squares =>
            board_tmp <- board;
            blocked_path =
              let exister = @list_exists (Square) in
              let occupied_predicate =
                fun (square : Square) =>
                  match square with
                  | Square row column =>
                    let row_contents = builtin get board_tmp row in
                    match row_contents with
                    | None => False
                    | Some row_map =>
                      let contents = builtin get row_map column in
                      match contents with
                      | Some (Occupied _ _ _) => True
                      | Some Free => False
                      | None => False
                      end
                    end
                  end
              in
              exister occupied_predicate intervening_squares;
            match blocked_path with
            | True => IllegalActionEvent
            | False =>
              contents_at_target <- board[target_row][target_column];
              match contents_at_target with
              | None => IllegalActionEvent
              | Some Free =>
                PerformMoveAndPromote
                  current_player piece promote promotion_status row column target_row target_column player_1_moves
              | Some (Occupied captured_piece _ captured_owner) =>
                captured_owner_is_current_player =
                  builtin eq captured_owner current_player;
                match captured_owner_is_current_player with
                | True => IllegalActionEvent
                | False =>
                  PerformMoveAndPromote
                    current_player piece promote promotion_status row column target_row target_column player_1_moves;
                  match captured_piece with
                  | King => Winner current_player
                  | _ =>
                    captured_piece_no = piece_to_int captured_piece;
                    captured_count_opt <-
                      captured_pieces[current_player][captured_piece_no];
                    match captured_count_opt with
                    | None => InternalErrorEvent
                    | Some count =>
                      one = Uint32 1;
                      new_captured_count = builtin add count one;
                      captured_pieces[current_player][captured_piece_no] :=
                        new_captured_count
                    end
                  end
                end
              end
            end
          end
        end
      | _ => IllegalActionEvent
      end
    end
  end
  
  transition MoveAction
    (
      row : Uint32,
      column : Uint32,
      direction : Direction,
      distance : Uint32,
      promote : Bool
    )
    square = Square row column;
    zero = Uint128 0;
    move = Move square direction distance promote;
    msg =
      {
        _tag : "PlayerAction";
        _amount : zero;
        _recipient : _this_address;
        action : move
      };
    nil_msg = Nil {(Message)};
    msgs = Cons {(Message)} msg nil_msg;
    send msgs
  end
  
  transition PlayerAction (action : Action)
    false = False;
    true = True;
    current_player_opt <- player_in_turn;
    match current_player_opt with
    | None =>
      err = game_is_over;
      e = mk_error_event err;
      event e
    | Some current_player =>
      correct_player = builtin eq _sender current_player;
      match correct_player with
      | False =>
        err = playing_out_of_turn;
        e = mk_error_event err;
        event e
      | True =>
        match action with
        | Resign => Resign current_player
        | Place piece square => PlacePiece piece square
        | Move square direction distance promote =>
          MovePiece current_player square direction distance promote
        end
      end;
      win <- winner;
      match win with
      | Some player =>
        e = mk_winner_event player player1 player2;
        event e
      | None =>
        next_player = get_next_player current_player player1 player2;
        next_player_opt = Some {(ByStr20)} next_player;
        player_in_turn := next_player_opt
      end
    end
  end
  
