
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR
    | UIDENT of (
# 15 "parser.mly"
       (string)
# 13 "parser.ml"
  )
    | TVOID
    | TSTRING
    | TRUE
    | TINT
    | TILDE
    | TBOOL
    | STRUCT
    | STRING of (
# 13 "parser.mly"
       (string)
# 25 "parser.ml"
  )
    | STAR
    | SEMI
    | RPAREN
    | RETURN
    | RBRACKET
    | RBRACE
    | QUESTION
    | PLUS
    | NULL
    | NEW
    | LTLT
    | LTEQ
    | LT
    | LPAREN
    | LENGTH
    | LBRACKET
    | LBRACE
    | IOR
    | INT of (
# 11 "parser.mly"
       (int64)
# 48 "parser.ml"
  )
    | IFQ
    | IF
    | IDENT of (
# 14 "parser.mly"
       (string)
# 55 "parser.ml"
  )
    | IAND
    | GTGTGT
    | GTGT
    | GTEQ
    | GT
    | GLOBAL
    | FOR
    | FALSE
    | EQEQ
    | EQ
    | EOF
    | ELSE
    | DOT
    | DASH
    | COMMA
    | BAR
    | BANGEQ
    | BANG
    | ARROW
    | AMPER
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState391
  | MenhirState389
  | MenhirState384
  | MenhirState382
  | MenhirState376
  | MenhirState371
  | MenhirState369
  | MenhirState366
  | MenhirState358
  | MenhirState349
  | MenhirState346
  | MenhirState341
  | MenhirState334
  | MenhirState332
  | MenhirState331
  | MenhirState329
  | MenhirState327
  | MenhirState320
  | MenhirState316
  | MenhirState309
  | MenhirState302
  | MenhirState296
  | MenhirState292
  | MenhirState287
  | MenhirState284
  | MenhirState279
  | MenhirState277
  | MenhirState273
  | MenhirState271
  | MenhirState267
  | MenhirState266
  | MenhirState264
  | MenhirState261
  | MenhirState258
  | MenhirState257
  | MenhirState255
  | MenhirState251
  | MenhirState250
  | MenhirState248
  | MenhirState245
  | MenhirState242
  | MenhirState240
  | MenhirState239
  | MenhirState237
  | MenhirState233
  | MenhirState232
  | MenhirState230
  | MenhirState227
  | MenhirState223
  | MenhirState222
  | MenhirState220
  | MenhirState216
  | MenhirState215
  | MenhirState213
  | MenhirState210
  | MenhirState208
  | MenhirState206
  | MenhirState205
  | MenhirState203
  | MenhirState196
  | MenhirState195
  | MenhirState193
  | MenhirState191
  | MenhirState190
  | MenhirState189
  | MenhirState187
  | MenhirState184
  | MenhirState179
  | MenhirState177
  | MenhirState173
  | MenhirState171
  | MenhirState169
  | MenhirState168
  | MenhirState163
  | MenhirState160
  | MenhirState157
  | MenhirState150
  | MenhirState143
  | MenhirState141
  | MenhirState138
  | MenhirState126
  | MenhirState123
  | MenhirState121
  | MenhirState116
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState67
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState56
  | MenhirState48
  | MenhirState43
  | MenhirState41
  | MenhirState36
  | MenhirState31
  | MenhirState27
  | MenhirState25
  | MenhirState19
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState9
  | MenhirState0

# 1 "parser.mly"
  
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }


# 223 "parser.ml"

let rec _menhir_goto_if_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.stmt Ast.node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_ifs_ = _endpos in
        let (ifs : (Ast.stmt Ast.node)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos_ifs_ in
        let _v : (Ast.block) = 
# 239 "parser.mly"
                      ( [ ifs ] )
# 239 "parser.ml"
         in
        _menhir_goto_else_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState391 | MenhirState169 | MenhirState302 | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_ifs_ = _endpos in
        let (ifs : (Ast.stmt Ast.node)) = _v in
        let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 250 "parser.ml"
        ) = 
# 219 "parser.mly"
                        ( ifs )
# 254 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce144 : _menhir_env -> (((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 263 "parser.ml"
) * Lexing.position)) * _menhir_state * (Ast.ty list)) * Lexing.position)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 269 "parser.ml"
    )), _startpos_t0_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, _startpos__100_) = _menhir_stack in
    let _100 = () in
    let _60 = () in
    let _50 = () in
    let _30 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 280 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _6 = _60 in
      let _5 = _50 in
      let xs0 = xs00 in
      let _3 = _30 in
      let t = t0 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 294 "parser.ml"
        
      in
      let l =
        let xs = xs0 in
        
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 302 "parser.ml"
        
      in
      
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 308 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 314 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run50 : _menhir_env -> (((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 321 "parser.ml"
) * Lexing.position)) * _menhir_state * (Ast.ty list)) * Lexing.position)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 329 "parser.ml"
    )), _startpos_t0_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, _startpos__100_) = _menhir_stack in
    let _2 = () in
    let _100 = () in
    let _60 = () in
    let _50 = () in
    let _30 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 341 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _6 = _60 in
      let _5 = _50 in
      let xs0 = xs00 in
      let _3 = _30 in
      let t = t0 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 355 "parser.ml"
        
      in
      let l =
        let xs = xs0 in
        
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 363 "parser.ml"
        
      in
      
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 369 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 375 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_else_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (Ast.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 390 "parser.ml"
        )), _startpos_e_), _endpos__4_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 394 "parser.ml"
        ))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 232 "parser.mly"
    ( loc _startpos _endpos @@ If(e,b1,b2) )
# 405 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (id0 : (
# 15 "parser.mly"
       (string)
# 416 "parser.ml"
        )), _startpos_id0_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 420 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 424 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 428 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let id = id0 in
          
# 134 "parser.mly"
              ( RStruct id )
# 440 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 448 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__10_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 459 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 463 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 467 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _10 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let _1 = _10 in
          
# 132 "parser.mly"
            ( RString )
# 480 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 488 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__11_), _endpos__20_, _), _, _startpos__100_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 499 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 503 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 507 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _100 = () in
        let _30 = () in
        let _20 = () in
        let _11 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let _10 = _100 in
          let _3 = _30 in
          let _2 = _20 in
          let _1 = _11 in
          let ret =
            let _1 = _10 in
            
# 128 "parser.mly"
           ( RetVoid )
# 528 "parser.ml"
            
          in
          
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 534 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 542 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__10_), _endpos__20_, _), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 553 "parser.ml"
        )), _startpos_t00_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 557 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 561 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 565 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _30 = () in
        let _20 = () in
        let _10 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let t0 = t00 in
          let _3 = _30 in
          let _2 = _20 in
          let _1 = _10 in
          let ret =
            let t = t0 in
            
# 129 "parser.mly"
           ( RetVal t )
# 585 "parser.ml"
            
          in
          
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 591 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 599 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let ((((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 610 "parser.ml"
        )), _startpos_t0_), _endpos__30_), _, _startpos__100_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 614 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 618 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 622 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _100 = () in
        let _40 = () in
        let _30 = () in
        let _11 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let _10 = _100 in
          let _4 = _40 in
          let _3 = _30 in
          let t = t0 in
          let _1 = _11 in
          let ret =
            let _1 = _10 in
            
# 128 "parser.mly"
           ( RetVoid )
# 644 "parser.ml"
            
          in
          
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 650 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 658 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let ((((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 669 "parser.ml"
        )), _startpos_t1_), _endpos__30_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 673 "parser.ml"
        )), _startpos_t00_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 677 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 681 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 685 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _40 = () in
        let _30 = () in
        let _10 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let t0 = t00 in
          let _4 = _40 in
          let _3 = _30 in
          let t = t1 in
          let _1 = _10 in
          let ret =
            let t = t0 in
            
# 129 "parser.mly"
           ( RetVal t )
# 706 "parser.ml"
            
          in
          
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 712 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 720 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 731 "parser.ml"
        )), _startpos_t0_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, _startpos__100_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 735 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 739 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 743 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _100 = () in
        let _60 = () in
        let _50 = () in
        let _30 = () in
        let _11 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let _10 = _100 in
          let _6 = _60 in
          let _5 = _50 in
          let xs0 = xs00 in
          let _3 = _30 in
          let t = t0 in
          let _1 = _11 in
          let ret =
            let _1 = _10 in
            
# 128 "parser.mly"
           ( RetVoid )
# 768 "parser.ml"
            
          in
          let l =
            let xs = xs0 in
            
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 776 "parser.ml"
            
          in
          
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 782 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 790 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let (((((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 801 "parser.ml"
        )), _startpos_t1_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 805 "parser.ml"
        )), _startpos_t00_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 809 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 813 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 817 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _60 = () in
        let _50 = () in
        let _30 = () in
        let _10 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let t0 = t00 in
          let _6 = _60 in
          let _5 = _50 in
          let xs0 = xs00 in
          let _3 = _30 in
          let t = t1 in
          let _1 = _10 in
          let ret =
            let t = t0 in
            
# 129 "parser.mly"
           ( RetVal t )
# 841 "parser.ml"
            
          in
          let l =
            let xs = xs0 in
            
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 849 "parser.ml"
            
          in
          
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 855 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 863 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_b2_ = _endpos in
        let (b2 : (Ast.block)) = _v in
        let ((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 874 "parser.ml"
        )), _startpos_t0_), _endpos__30_, _), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 878 "parser.ml"
        )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 882 "parser.ml"
        )), _startpos_e_), _endpos__7_), _endpos_b1_, _, (b1 : (
# 93 "parser.mly"
      (Ast.block)
# 886 "parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _30 = () in
        let _20 = () in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos_b2_ in
        let _v : (Ast.stmt Ast.node) = let r =
          let _3 = _30 in
          let _2 = _20 in
          let t = t0 in
          
# 133 "parser.mly"
                           ( RArray t )
# 902 "parser.ml"
          
        in
        let _endpos = _endpos_b2_ in
        let _startpos = _startpos__1_ in
        
# 234 "parser.mly"
    ( loc _startpos _endpos @@ Cast(r, id, e, b1, b2) )
# 910 "parser.ml"
         in
        _menhir_goto_if_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : (Ast.block) = 
# 237 "parser.mly"
                      ( [] )
# 922 "parser.ml"
     in
    _menhir_goto_else_stmt _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run191 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IFQ ->
        _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191

and _menhir_reduce32 : _menhir_env -> ((('ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 946 "parser.ml"
) * Lexing.position) * Lexing.position) * _menhir_state * (Ast.exp Ast.node list)) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 952 "parser.ml"
    )), _startpos_e_), _startpos__2_), _, (xs0 : (Ast.exp Ast.node list))), _endpos__4_) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos__4_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 961 "parser.ml"
    ) = let es =
      let xs = xs0 in
      
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 967 "parser.ml"
      
    in
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_e_ in
    
# 201 "parser.mly"
                        ( loc _startpos _endpos @@ Call (e,es) )
# 975 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_COMMA_gexp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState371 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp Ast.node list)) = _v in
        let _v : (Ast.exp Ast.node list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 989 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_gexp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState376 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp Ast.node list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.node))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp Ast.node list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1001 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_gexp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_gfield_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.id * Ast.exp Ast.node) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.id * Ast.exp Ast.node) list)) = _v in
        let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 1017 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMI_gfield__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState366 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.id * Ast.exp Ast.node) list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.id * Ast.exp Ast.node))), _endpos__2_) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1029 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_gfield_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState48 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | QUESTION ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
                    | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
                        _menhir_reduce144 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState126 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NULL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__2_ = _endpos in
                        let (((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 1142 "parser.ml"
                        )), _startpos_t0_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, _startpos__100_) = _menhir_stack in
                        let _2 = () in
                        let _100 = () in
                        let _60 = () in
                        let _50 = () in
                        let _30 = () in
                        let _11 = () in
                        let _startpos = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 1155 "parser.ml"
                        ) = let r =
                          let _10 = _100 in
                          let _6 = _60 in
                          let _5 = _50 in
                          let xs0 = xs00 in
                          let _3 = _30 in
                          let t = t0 in
                          let _1 = _11 in
                          let ret =
                            let _1 = _10 in
                            
# 128 "parser.mly"
           ( RetVoid )
# 1169 "parser.ml"
                            
                          in
                          let l =
                            let xs = xs0 in
                            
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1177 "parser.ml"
                            
                          in
                          
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 1183 "parser.ml"
                          
                        in
                        let _startpos_r_ = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _startpos = _startpos_r_ in
                        
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 1192 "parser.ml"
                         in
                        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                    | QUESTION ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
                    | COMMA | LBRACKET | RPAREN ->
                        _menhir_reduce144 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState245 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENT _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQ ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | BANG ->
                                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | DASH ->
                                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | FALSE ->
                                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | INT _v ->
                                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LENGTH ->
                                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | NEW ->
                                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | STRING _v ->
                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TBOOL ->
                                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TILDE ->
                                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TINT ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TRUE ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TSTRING ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | UIDENT _v ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | QUESTION ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
                    | LBRACKET ->
                        _menhir_reduce144 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState245)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState346 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState349 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NULL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__2_ = _endpos in
                        let (((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 1379 "parser.ml"
                        )), _startpos_t0_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, _startpos__100_) = _menhir_stack in
                        let _2 = () in
                        let _100 = () in
                        let _60 = () in
                        let _50 = () in
                        let _30 = () in
                        let _11 = () in
                        let _v : (Ast.exp Ast.node) = let r =
                          let _10 = _100 in
                          let _6 = _60 in
                          let _5 = _50 in
                          let xs0 = xs00 in
                          let _3 = _30 in
                          let t = t0 in
                          let _1 = _11 in
                          let ret =
                            let _1 = _10 in
                            
# 128 "parser.mly"
           ( RetVoid )
# 1400 "parser.ml"
                            
                          in
                          let l =
                            let xs = xs0 in
                            
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1408 "parser.ml"
                            
                          in
                          
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 1414 "parser.ml"
                          
                        in
                        let _startpos_r_ = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _startpos = _startpos_r_ in
                        
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 1423 "parser.ml"
                         in
                        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
                    | QUESTION ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
                    | LBRACKET ->
                        _menhir_reduce144 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState349)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 1467 "parser.ml"
        ))), _, (xs : (Ast.block))) = _menhir_stack in
        let _v : (Ast.block) = 
# 201 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1472 "parser.ml"
         in
        _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s), _, (stmts : (Ast.block))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos__3_ in
            let _v : (
# 93 "parser.mly"
      (Ast.block)
# 1493 "parser.ml"
            ) = 
# 228 "parser.mly"
                                   ( stmts )
# 1497 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState173 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 1507 "parser.ml"
                )), _startpos_e_), _endpos__4_), _endpos_b_, _, (b : (
# 93 "parser.mly"
      (Ast.block)
# 1511 "parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 1519 "parser.ml"
                ) = let _endpos = _endpos_b_ in
                let _startpos = _startpos__1_ in
                
# 223 "parser.mly"
                        ( loc _startpos _endpos @@ While(e, b) )
# 1525 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | MenhirState189 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
            | MenhirState195 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
            | MenhirState191 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _endpos_b_, _, (b : (
# 93 "parser.mly"
      (Ast.block)
# 1560 "parser.ml"
                ))) = _menhir_stack in
                let _1 = () in
                let _endpos = _endpos_b_ in
                let _v : (Ast.block) = 
# 238 "parser.mly"
                      ( b )
# 1567 "parser.ml"
                 in
                _menhir_goto_else_stmt _menhir_env _menhir_stack _endpos _menhir_s _v
            | MenhirState205 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState206
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState206
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
            | MenhirState215 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState216
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState216
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
            | MenhirState222 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState223
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState223
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState223)
            | MenhirState232 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState233
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState233
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233)
            | MenhirState239 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState240
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState240
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
            | MenhirState250 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState251
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState251
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251)
            | MenhirState257 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState258
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
            | MenhirState266 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState267
                | BANG | DASH | EOF | FALSE | FOR | IDENT _ | IF | IFQ | INT _ | LENGTH | LPAREN | NEW | RBRACE | RETURN | RPAREN | STRING _ | TBOOL | TILDE | TINT | TRUE | TSTRING | UIDENT _ | VAR | WHILE ->
                    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState267
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267)
            | MenhirState284 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (xs0 : (Ast.vdecl list))), _endpos__4_), _, (e : (Ast.exp Ast.node option))), _endpos__6_), _, (s : (Ast.stmt Ast.node option))), _endpos__8_), _endpos_b_, _, (b : (
# 93 "parser.mly"
      (Ast.block)
# 1680 "parser.ml"
                ))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 1690 "parser.ml"
                ) = let ds =
                  let xs = xs0 in
                  
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1696 "parser.ml"
                  
                in
                let _endpos = _endpos_b_ in
                let _startpos = _startpos__1_ in
                
# 225 "parser.mly"
                        ( loc _startpos _endpos @@ For(ds,e,s,b) )
# 1704 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | MenhirState168 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s, _startpos__10_), _endpos_fname_, (fname : (
# 14 "parser.mly"
       (string)
# 1713 "parser.ml"
                )), _startpos_fname_), _startpos__3_), _, (args : ((Ast.ty * Ast.id) list))), _endpos__5_), _endpos_body_, _, (body : (
# 93 "parser.mly"
      (Ast.block)
# 1717 "parser.ml"
                ))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _10 = () in
                let _v : (Ast.decl) = let frtyp =
                  let _1 = _10 in
                  
# 128 "parser.mly"
           ( RetVoid )
# 1727 "parser.ml"
                  
                in
                let _startpos_frtyp_ = _startpos__10_ in
                let _endpos = _endpos_body_ in
                let _startpos = _startpos_frtyp_ in
                
# 110 "parser.mly"
    ( Gfdecl (loc _startpos _endpos { frtyp; fname; args; body }) )
# 1736 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | MenhirState384 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 1745 "parser.ml"
                )), _startpos_t0_), _endpos_fname_, (fname : (
# 14 "parser.mly"
       (string)
# 1749 "parser.ml"
                )), _startpos_fname_), _startpos__3_), _, (args : ((Ast.ty * Ast.id) list))), _endpos__5_), _endpos_body_, _, (body : (
# 93 "parser.mly"
      (Ast.block)
# 1753 "parser.ml"
                ))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _v : (Ast.decl) = let frtyp =
                  let t = t0 in
                  
# 129 "parser.mly"
           ( RetVal t )
# 1762 "parser.ml"
                  
                in
                let _startpos_frtyp_ = _startpos_t0_ in
                let _endpos = _endpos_body_ in
                let _startpos = _startpos_frtyp_ in
                
# 110 "parser.mly"
    ( Gfdecl (loc _startpos _endpos { frtyp; fname; args; body }) )
# 1771 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__7_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (
# 94 "parser.mly"
      (Ast.ty)
# 1820 "parser.ml"
            )), _startpos_t_), _endpos__4_, _), _, (xs0 : (Ast.exp Ast.node list))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__7_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 1832 "parser.ml"
            ) = let cs =
              let xs = xs0 in
              
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1838 "parser.ml"
              
            in
            let _endpos = _endpos__7_ in
            let _startpos = _startpos__1_ in
            
# 191 "parser.mly"
                        ( loc _startpos _endpos @@ CArr (t, cs) )
# 1846 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState292 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__5_ = _endpos in
                let ((((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 1876 "parser.ml"
                )), _startpos_e_), _startpos__2_), _, (xs0 : (Ast.exp Ast.node list))), _endpos__4_) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 1884 "parser.ml"
                ) = let es =
                  let xs = xs0 in
                  
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1890 "parser.ml"
                  
                in
                let _endpos = _endpos__5_ in
                let _startpos = _startpos_e_ in
                
# 218 "parser.mly"
                        ( loc _startpos _endpos @@ SCall (e, es) )
# 1898 "parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
            | AMPER | BANGEQ | BAR | DASH | DOT | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LBRACKET | LPAREN | LT | LTEQ | LTLT | PLUS | STAR ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_pair_ty_IDENT___ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ty * Ast.id) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : ((Ast.ty * Ast.id) list)) = _v in
    let _v : ((Ast.ty * Ast.id) list) = let l =
      let xs = xs0 in
      
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1928 "parser.ml"
      
    in
    
# 118 "parser.mly"
                                            ( l )
# 1934 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState382 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState384
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GLOBAL ->
        _menhir_run318 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRUCT ->
        _menhir_run307 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TVOID ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState389 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState389
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState389

and _menhir_goto_loption_separated_nonempty_list_SEMI_gfield__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.id * Ast.exp Ast.node) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__5_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, (i : (
# 15 "parser.mly"
       (string)
# 2035 "parser.ml"
        )), _startpos_i_), _, (xs0 : ((Ast.id * Ast.exp Ast.node) list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp Ast.node) = let fs =
          let xs = xs0 in
          
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2045 "parser.ml"
          
        in
        let _endpos = _endpos__5_ in
        let _startpos = _startpos__1_ in
        
# 172 "parser.mly"
               ( loc _startpos _endpos @@ CStruct (i, fs) )
# 2053 "parser.ml"
         in
        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run330 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 2066 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run355 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState331 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run354 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState331 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run332 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run327 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run326 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState331 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run325 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState331)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_gexp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__7_ = _endpos in
        let ((((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (
# 94 "parser.mly"
      (Ast.ty)
# 2127 "parser.ml"
        )), _startpos_t_), _endpos__4_, _), _, (xs0 : (Ast.exp Ast.node list))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp Ast.node) = let cs =
          let xs = xs0 in
          
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2139 "parser.ml"
          
        in
        let _endpos = _endpos__7_ in
        let _startpos = _startpos__1_ in
        
# 170 "parser.mly"
               ( loc _startpos _endpos @@ CArr (t, cs) )
# 2147 "parser.ml"
         in
        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_gexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState331 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 14 "parser.mly"
       (string)
# 2167 "parser.ml"
        )), _startpos_id_), _, (e : (Ast.exp Ast.node))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.id * Ast.exp Ast.node) = 
# 176 "parser.mly"
                       ( (id, e) )
# 2173 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState366 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState366)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.id * Ast.exp Ast.node))) = _menhir_stack in
            let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2199 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMI_gfield_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState376 | MenhirState371 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run355 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run354 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run332 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run327 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run326 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run325 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState376 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState376)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp Ast.node))) = _menhir_stack in
            let _v : (Ast.exp Ast.node list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2250 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_gexp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_name_, (name : (
# 14 "parser.mly"
       (string)
# 2273 "parser.ml"
            )), _startpos_name_), _, (init : (Ast.exp Ast.node))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.decl) = let _endpos = _endpos__5_ in
            let _startpos = _startpos__1_ in
            
# 108 "parser.mly"
    ( Gvdecl (loc _startpos _endpos { name; init }) )
# 2283 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_decl_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.field list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.field list)) = _v in
        let _v : (Ast.field list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 2305 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMI_decl_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.field list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.field))), _endpos__2_) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.field list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2317 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_decl_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_pair_ty_IDENT__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ty * Ast.id) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.ty * Ast.id) list)) = _v in
        let ((_menhir_stack, _menhir_s, (x0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2333 "parser.ml"
        )), _startpos_x0_), _endpos_y0_, (y0 : (
# 14 "parser.mly"
       (string)
# 2337 "parser.ml"
        )), _startpos_y0_) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.ty * Ast.id) list) = let x =
          let y = y0 in
          let x = x0 in
          
# 155 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( (x, y) )
# 2346 "parser.ml"
          
        in
        
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2352 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_pair_ty_IDENT__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState382 | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.ty * Ast.id) list)) = _v in
        let _v : ((Ast.ty * Ast.id) list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 2362 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pair_ty_IDENT___ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce138 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2371 "parser.ml"
) * Lexing.position)) * Lexing.position * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2377 "parser.ml"
    )), _startpos_t0_), _endpos__30_, _) = _menhir_stack in
    let _30 = () in
    let _20 = () in
    let _startpos = _startpos_t0_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2385 "parser.ml"
    ) = let r =
      let _3 = _30 in
      let _2 = _20 in
      let t = t0 in
      
# 133 "parser.mly"
                           ( RArray t )
# 2393 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 2399 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run33 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2406 "parser.ml"
) * Lexing.position)) * Lexing.position * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2414 "parser.ml"
    )), _startpos_t0_), _endpos__30_, _) = _menhir_stack in
    let _2 = () in
    let _30 = () in
    let _20 = () in
    let _startpos = _startpos_t0_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2423 "parser.ml"
    ) = let r =
      let _3 = _30 in
      let _2 = _20 in
      let t = t0 in
      
# 133 "parser.mly"
                           ( RArray t )
# 2431 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 2437 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run65 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2444 "parser.ml"
) * Lexing.position)) * Lexing.position * _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__2_ = _endpos in
    let ((_menhir_stack, _menhir_s, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2453 "parser.ml"
    )), _startpos_t0_), _endpos__30_, _) = _menhir_stack in
    let _2 = () in
    let _30 = () in
    let _20 = () in
    let _startpos = _startpos_t0_ in
    let _endpos = _endpos__2_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 2463 "parser.ml"
    ) = let r =
      let _3 = _30 in
      let _2 = _20 in
      let t = t0 in
      
# 133 "parser.mly"
                           ( RArray t )
# 2471 "parser.ml"
      
    in
    let _startpos_r_ = _startpos_t0_ in
    let _endpos = _endpos__2_ in
    let _startpos = _startpos_r_ in
    
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 2480 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce145 : _menhir_env -> (((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2487 "parser.ml"
) * Lexing.position)) * _menhir_state * (Ast.ty list)) * Lexing.position)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2491 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 2497 "parser.ml"
    )), _startpos_t1_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2501 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _60 = () in
    let _50 = () in
    let _30 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2511 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _6 = _60 in
      let _5 = _50 in
      let xs0 = xs00 in
      let _3 = _30 in
      let t = t1 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2525 "parser.ml"
        
      in
      let l =
        let xs = xs0 in
        
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2533 "parser.ml"
        
      in
      
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 2539 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 2545 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run52 : _menhir_env -> (((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2552 "parser.ml"
) * Lexing.position)) * _menhir_state * (Ast.ty list)) * Lexing.position)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2556 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 2564 "parser.ml"
    )), _startpos_t1_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2568 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _2 = () in
    let _60 = () in
    let _50 = () in
    let _30 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2579 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _6 = _60 in
      let _5 = _50 in
      let xs0 = xs00 in
      let _3 = _30 in
      let t = t1 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2593 "parser.ml"
        
      in
      let l =
        let xs = xs0 in
        
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2601 "parser.ml"
        
      in
      
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 2607 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 2613 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_COMMA_ty_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.ty list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 94 "parser.mly"
      (Ast.ty)
# 2627 "parser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ty list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2633 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState346 | MenhirState242 | MenhirState123 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.ty list)) = _v in
        let _v : (Ast.ty list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 2643 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce143 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2652 "parser.ml"
) * Lexing.position) * Lexing.position)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2656 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 2662 "parser.ml"
    )), _startpos_t1_), _endpos__30_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2666 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _40 = () in
    let _30 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2675 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _4 = _40 in
      let _3 = _30 in
      let t = t1 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2687 "parser.ml"
        
      in
      
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 2693 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 2699 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run40 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2706 "parser.ml"
) * Lexing.position) * Lexing.position)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2710 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 2718 "parser.ml"
    )), _startpos_t1_), _endpos__30_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2722 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _2 = () in
    let _40 = () in
    let _30 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2732 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _4 = _40 in
      let _3 = _30 in
      let t = t1 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2744 "parser.ml"
        
      in
      
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 2750 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 2756 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce155 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2763 "parser.ml"
) * Lexing.position) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (
# 94 "parser.mly"
      (Ast.ty)
# 2769 "parser.ml"
    )), _startpos_t_), _endpos__3_) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2777 "parser.ml"
    ) = 
# 124 "parser.mly"
                       ( t )
# 2781 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce142 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2788 "parser.ml"
) * Lexing.position) * Lexing.position)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2794 "parser.ml"
    )), _startpos_t0_), _endpos__30_), _, _startpos__100_) = _menhir_stack in
    let _100 = () in
    let _40 = () in
    let _30 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2804 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _4 = _40 in
      let _3 = _30 in
      let t = t0 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 2816 "parser.ml"
        
      in
      
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 2822 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 2828 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run38 : _menhir_env -> (((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2835 "parser.ml"
) * Lexing.position) * Lexing.position)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 2843 "parser.ml"
    )), _startpos_t0_), _endpos__30_), _, _startpos__100_) = _menhir_stack in
    let _2 = () in
    let _100 = () in
    let _40 = () in
    let _30 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2854 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _4 = _40 in
      let _3 = _30 in
      let t = t0 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 2866 "parser.ml"
        
      in
      
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 2872 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 2878 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.ty list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2887 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce141 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2894 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, _startpos__10_), _endpos__20_, _), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2900 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _30 = () in
    let _20 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2909 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _3 = _30 in
      let _2 = _20 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2920 "parser.ml"
        
      in
      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 2926 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 2932 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run30 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state)) * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2939 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((_menhir_stack, _menhir_s, _startpos__10_), _endpos__20_, _), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 2947 "parser.ml"
    )), _startpos_t00_) = _menhir_stack in
    let _2 = () in
    let _30 = () in
    let _20 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 2957 "parser.ml"
    ) = let r =
      let t0 = t00 in
      let _3 = _30 in
      let _2 = _20 in
      let _1 = _10 in
      let ret =
        let t = t0 in
        
# 129 "parser.mly"
           ( RetVal t )
# 2968 "parser.ml"
        
      in
      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 2974 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 2980 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 94 "parser.mly"
      (Ast.ty)
# 2987 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState31 in
        let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | QUESTION ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
            _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.block) = 
# 199 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3021 "parser.ml"
     in
    _menhir_goto_list_stmt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_stmt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stmt Ast.node option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState284
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState284)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState279 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IFQ ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState279 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState279 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VAR ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState279 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState279 in
            let _v : (Ast.stmt Ast.node option) = 
# 114 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( None )
# 3114 "parser.ml"
             in
            _menhir_goto_option_stmt_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState279)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.exp Ast.node list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3133 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> (('ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3140 "parser.ml"
) * Lexing.position)) * Lexing.position * (
# 14 "parser.mly"
       (string)
# 3144 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3150 "parser.ml"
    )), _startpos_e_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 3154 "parser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_id_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3162 "parser.ml"
    ) = let _endpos = _endpos_id_ in
    let _startpos = _startpos_e_ in
    
# 194 "parser.mly"
                        ( loc _startpos _endpos @@ Proj(e, id) )
# 3168 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_vdecl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.vdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.vdecl list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.vdecl)), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.vdecl list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3189 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_vdecl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.vdecl list)) = _v in
        let _v : (Ast.vdecl list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 3199 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_vdecl__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run169 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IFQ ->
        _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_goto_separated_nonempty_list_SEMI_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.id * Ast.exp Ast.node) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.id * Ast.exp Ast.node) list)) = _v in
        let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 3270 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMI_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.id * Ast.exp Ast.node) list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.id * Ast.exp Ast.node))), _endpos__2_) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3282 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMI_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> ((('ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3291 "parser.ml"
) * Lexing.position)) * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3295 "parser.ml"
) * Lexing.position) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3301 "parser.ml"
    )), _startpos_e_), _endpos_i_, _, (i : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3305 "parser.ml"
    )), _startpos_i_), _endpos__4_) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos__4_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3314 "parser.ml"
    ) = let _endpos = _endpos__4_ in
    let _startpos = _startpos_e_ in
    
# 199 "parser.mly"
                        ( loc _startpos _endpos @@ Index (e, i) )
# 3320 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState292 | MenhirState143 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp Ast.node list)) = _v in
        let _v : (Ast.exp Ast.node list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 3334 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp Ast.node list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3344 "parser.ml"
        )), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp Ast.node list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3350 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run72 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3359 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3403 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3447 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3491 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run91 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3535 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3579 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run95 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3623 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3667 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3711 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run97 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3755 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3799 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3843 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3887 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3910 "parser.ml"
) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run103 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3955 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run105 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 3999 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run107 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 4043 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run67 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 4087 "parser.ml"
) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run74 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 4134 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (p : (Ast.prog))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 90 "parser.mly"
      (Ast.prog)
# 4192 "parser.ml"
            ) = 
# 104 "parser.mly"
                      ( p )
# 4196 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 90 "parser.mly"
      (Ast.prog)
# 4203 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState389 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.decl))), _, (xs : (Ast.prog))) = _menhir_stack in
        let _v : (Ast.prog) = 
# 201 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 4219 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ast.ty * Ast.id) list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 4230 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_pair_ty_IDENT___ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_SEMI_decl_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.field list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__5_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), (name : (
# 15 "parser.mly"
       (string)
# 4250 "parser.ml"
        )), _startpos_name_), _, (xs0 : (Ast.field list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.decl) = let fs =
          let xs = xs0 in
          
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 4260 "parser.ml"
          
        in
        let _endpos = _endpos__5_ in
        let _startpos = _startpos__1_ in
        
# 112 "parser.mly"
    ( Gtdecl (loc _startpos _endpos (name, fs)) )
# 4268 "parser.ml"
         in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run321 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 4281 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NULL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, (id0 : (
# 15 "parser.mly"
       (string)
# 4297 "parser.ml"
        )), _startpos_id0_) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp Ast.node) = let r =
          let id = id0 in
          
# 134 "parser.mly"
              ( RStruct id )
# 4305 "parser.ml"
          
        in
        let _startpos_r_ = _startpos_id0_ in
        let _endpos = _endpos__2_ in
        let _startpos = _startpos_r_ in
        
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 4314 "parser.ml"
         in
        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
    | QUESTION ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
    | LBRACKET ->
        _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run323 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NULL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__10_) = _menhir_stack in
        let _2 = () in
        let _10 = () in
        let _v : (Ast.exp Ast.node) = let r =
          let _1 = _10 in
          
# 132 "parser.mly"
            ( RString )
# 4348 "parser.ml"
          
        in
        let _startpos_r_ = _startpos__10_ in
        let _endpos = _endpos__2_ in
        let _startpos = _startpos_r_ in
        
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 4357 "parser.ml"
         in
        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
    | QUESTION ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
    | LBRACKET ->
        _menhir_reduce137 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run325 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _v : (Ast.exp Ast.node) = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    
# 165 "parser.mly"
               ( loc _startpos _endpos @@ CBool true )
# 4383 "parser.ml"
     in
    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run326 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 13 "parser.mly"
       (string)
# 4390 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (
# 13 "parser.mly"
       (string)
# 4399 "parser.ml"
    )) = _v in
    let _startpos_s_ = _startpos in
    let _v : (Ast.exp Ast.node) = let _endpos = _endpos_s_ in
    let _startpos = _startpos_s_ in
    
# 168 "parser.mly"
               ( loc _startpos _endpos @@ CStr s )
# 4407 "parser.ml"
     in
    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run327 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState327 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run330 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState329 in
                let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 4446 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMI_gfield__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329)
        | QUESTION ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState327

and _menhir_run332 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState332 in
        let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TVOID ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState334 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NULL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos__2_ = _endpos in
                    let (((_menhir_stack, _menhir_s, _startpos__11_), _endpos__20_, _), _, _startpos__100_) = _menhir_stack in
                    let _2 = () in
                    let _100 = () in
                    let _30 = () in
                    let _20 = () in
                    let _11 = () in
                    let _v : (Ast.exp Ast.node) = let r =
                      let _10 = _100 in
                      let _3 = _30 in
                      let _2 = _20 in
                      let _1 = _11 in
                      let ret =
                        let _1 = _10 in
                        
# 128 "parser.mly"
           ( RetVoid )
# 4527 "parser.ml"
                        
                      in
                      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 4533 "parser.ml"
                      
                    in
                    let _startpos_r_ = _startpos__11_ in
                    let _endpos = _endpos__2_ in
                    let _startpos = _startpos_r_ in
                    
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 4542 "parser.ml"
                     in
                    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
                | QUESTION ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState332

and _menhir_run354 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 11 "parser.mly"
       (int64)
# 4583 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 11 "parser.mly"
       (int64)
# 4592 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _v : (Ast.exp Ast.node) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 167 "parser.mly"
               ( loc _startpos _endpos @@ CInt i )
# 4600 "parser.ml"
     in
    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run355 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 4607 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_id_ = _endpos in
    let (id : (
# 14 "parser.mly"
       (string)
# 4616 "parser.ml"
    )) = _v in
    let _startpos_id_ = _startpos in
    let _v : (Ast.exp Ast.node) = let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    
# 173 "parser.mly"
             (loc _startpos _endpos @@ Id id )
# 4624 "parser.ml"
     in
    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run356 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _v : (Ast.exp Ast.node) = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    
# 166 "parser.mly"
               ( loc _startpos _endpos @@ CBool false )
# 4640 "parser.ml"
     in
    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 94 "parser.mly"
      (Ast.ty)
# 4647 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | IDENT _ | NULL | RPAREN ->
            _menhir_reduce141 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState36 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | QUESTION ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
                    | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
                        _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
            | COMMA | IDENT _ | LBRACKET | NULL | QUESTION | RPAREN ->
                _menhir_reduce155 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | IDENT _ | NULL | RPAREN ->
            _menhir_reduce143 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState346 | MenhirState242 | MenhirState123 | MenhirState43 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (
# 94 "parser.mly"
      (Ast.ty)
# 4803 "parser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : (Ast.ty list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 4808 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | IDENT _ | NULL | RPAREN ->
            _menhir_reduce145 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__10_), _endpos__20_, _), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 4850 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _30 = () in
            let _20 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 4861 "parser.ml"
            ) = let r =
              let t0 = t00 in
              let _3 = _30 in
              let _2 = _20 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 4872 "parser.ml"
                
              in
              
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 4878 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 4887 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | QUESTION ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAREN ->
            _menhir_reduce141 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState391 | MenhirState302 | MenhirState169 | MenhirState296 | MenhirState292 | MenhirState287 | MenhirState279 | MenhirState277 | MenhirState264 | MenhirState255 | MenhirState248 | MenhirState237 | MenhirState230 | MenhirState220 | MenhirState213 | MenhirState203 | MenhirState193 | MenhirState187 | MenhirState179 | MenhirState177 | MenhirState171 | MenhirState0 | MenhirState9 | MenhirState150 | MenhirState141 | MenhirState143 | MenhirState16 | MenhirState56 | MenhirState60 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState77 | MenhirState74 | MenhirState72 | MenhirState67 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState63 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NULL ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState121 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NULL ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | COMMA | LBRACKET | RPAREN ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState116 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NULL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__2_ = _endpos in
                        let ((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 5032 "parser.ml"
                        )), _startpos_t0_), _endpos__30_), _, _startpos__100_) = _menhir_stack in
                        let _2 = () in
                        let _100 = () in
                        let _40 = () in
                        let _30 = () in
                        let _11 = () in
                        let _startpos = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 5044 "parser.ml"
                        ) = let r =
                          let _10 = _100 in
                          let _4 = _40 in
                          let _3 = _30 in
                          let t = t0 in
                          let _1 = _11 in
                          let ret =
                            let _1 = _10 in
                            
# 128 "parser.mly"
           ( RetVoid )
# 5056 "parser.ml"
                            
                          in
                          
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 5062 "parser.ml"
                          
                        in
                        let _startpos_r_ = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _startpos = _startpos_r_ in
                        
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 5071 "parser.ml"
                         in
                        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                    | QUESTION ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
                    | COMMA | LBRACKET | RPAREN ->
                        _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
            | COMMA | LBRACKET | RPAREN ->
                _menhir_reduce155 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 5120 "parser.ml"
            )), _startpos_t1_), _endpos__30_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 5124 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _40 = () in
            let _30 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 5135 "parser.ml"
            ) = let r =
              let t0 = t00 in
              let _4 = _40 in
              let _3 = _30 in
              let t = t1 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 5147 "parser.ml"
                
              in
              
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 5153 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 5162 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | QUESTION ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAREN ->
            _menhir_reduce143 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 5191 "parser.ml"
            )), _startpos_t1_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 5195 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _60 = () in
            let _50 = () in
            let _30 = () in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 5207 "parser.ml"
            ) = let r =
              let t0 = t00 in
              let _6 = _60 in
              let _5 = _50 in
              let xs0 = xs00 in
              let _3 = _30 in
              let t = t1 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 5221 "parser.ml"
                
              in
              let l =
                let xs = xs0 in
                
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 5229 "parser.ml"
                
              in
              
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 5235 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 5244 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | QUESTION ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAREN ->
            _menhir_reduce145 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DASH ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LENGTH ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState141 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BANG ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | DASH ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | FALSE ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState143 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INT _v ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState143 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LENGTH ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | NEW ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | STRING _v ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState143 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TILDE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TINT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TRUE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TSTRING ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | UIDENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | RBRACE ->
                        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | STRING _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TILDE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState382 | MenhirState163 | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (x0 : (
# 94 "parser.mly"
      (Ast.ty)
# 5403 "parser.ml"
                )), _startpos_x0_), _endpos_y0_, (y0 : (
# 14 "parser.mly"
       (string)
# 5407 "parser.ml"
                )), _startpos_y0_) = _menhir_stack in
                let _v : ((Ast.ty * Ast.id) list) = let x =
                  let y = y0 in
                  let x = x0 in
                  
# 155 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( (x, y) )
# 5415 "parser.ml"
                  
                in
                
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5421 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_pair_ty_IDENT__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BANG ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DASH ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LENGTH ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TILDE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState227 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENT _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQ ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | BANG ->
                                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | DASH ->
                                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | FALSE ->
                                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | INT _v ->
                                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LENGTH ->
                                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | NEW ->
                                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | STRING _v ->
                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TBOOL ->
                                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TILDE ->
                                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TINT ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TRUE ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | TSTRING ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | UIDENT _v ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | QUESTION ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
                    | LBRACKET ->
                        _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState227)
            | LBRACKET ->
                _menhir_reduce155 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BANG ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DASH ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LENGTH ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TILDE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState245 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BANG ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DASH ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FALSE ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState255 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState255 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LENGTH ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NEW ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState255 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TILDE ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TRUE ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState255)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | QUESTION ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState261 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BANG ->
                            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | DASH ->
                            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | FALSE ->
                            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | IDENT _v ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | INT _v ->
                            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LENGTH ->
                            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LPAREN ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | NEW ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | STRING _v ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TBOOL ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TILDE ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TINT ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TRUE ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TSTRING ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | UIDENT _v ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _, _menhir_s), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState261)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState316 | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_id_ = _endpos in
            let (id : (
# 14 "parser.mly"
       (string)
# 5887 "parser.ml"
            )) = _v in
            let _startpos_id_ = _startpos in
            let (_menhir_stack, _menhir_s, (t : (
# 94 "parser.mly"
      (Ast.ty)
# 5893 "parser.ml"
            )), _startpos_t_) = _menhir_stack in
            let _v : (Ast.field) = 
# 115 "parser.mly"
                  ( { fieldName=id; ftyp=t } )
# 5898 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState316)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.field))) = _menhir_stack in
                let _v : (Ast.field list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 5932 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_decl_field_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState334 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__10_), _endpos__20_, _), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 5965 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _30 = () in
            let _20 = () in
            let _10 = () in
            let _v : (Ast.exp Ast.node) = let r =
              let t0 = t00 in
              let _3 = _30 in
              let _2 = _20 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 5981 "parser.ml"
                
              in
              
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 5987 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 5996 "parser.ml"
             in
            _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
        | QUESTION ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState332 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack) MenhirState346
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState346)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TVOID ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState341 in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NULL ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__2_ = _endpos in
                        let ((((_menhir_stack, _menhir_s, _startpos__11_), _, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 6072 "parser.ml"
                        )), _startpos_t0_), _endpos__30_), _, _startpos__100_) = _menhir_stack in
                        let _2 = () in
                        let _100 = () in
                        let _40 = () in
                        let _30 = () in
                        let _11 = () in
                        let _v : (Ast.exp Ast.node) = let r =
                          let _10 = _100 in
                          let _4 = _40 in
                          let _3 = _30 in
                          let t = t0 in
                          let _1 = _11 in
                          let ret =
                            let _1 = _10 in
                            
# 128 "parser.mly"
           ( RetVoid )
# 6090 "parser.ml"
                            
                          in
                          
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 6096 "parser.ml"
                          
                        in
                        let _startpos_r_ = _startpos__11_ in
                        let _endpos = _endpos__2_ in
                        let _startpos = _startpos_r_ in
                        
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 6105 "parser.ml"
                         in
                        _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
                    | QUESTION ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
                    | LBRACKET ->
                        _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState341)
            | LBRACKET ->
                _menhir_reduce155 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 6154 "parser.ml"
            )), _startpos_t1_), _endpos__30_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 6158 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _40 = () in
            let _30 = () in
            let _10 = () in
            let _v : (Ast.exp Ast.node) = let r =
              let t0 = t00 in
              let _4 = _40 in
              let _3 = _30 in
              let t = t1 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 6175 "parser.ml"
                
              in
              
# 136 "parser.mly"
                                        ( RFun ([t], ret) )
# 6181 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 6190 "parser.ml"
             in
            _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
        | QUESTION ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState349 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NULL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (t1 : (
# 94 "parser.mly"
      (Ast.ty)
# 6217 "parser.ml"
            )), _startpos_t1_), _, (xs00 : (Ast.ty list))), _endpos__50_), _, (t00 : (
# 94 "parser.mly"
      (Ast.ty)
# 6221 "parser.ml"
            )), _startpos_t00_) = _menhir_stack in
            let _2 = () in
            let _60 = () in
            let _50 = () in
            let _30 = () in
            let _10 = () in
            let _v : (Ast.exp Ast.node) = let r =
              let t0 = t00 in
              let _6 = _60 in
              let _5 = _50 in
              let xs0 = xs00 in
              let _3 = _30 in
              let t = t1 in
              let _1 = _10 in
              let ret =
                let t = t0 in
                
# 129 "parser.mly"
           ( RetVal t )
# 6241 "parser.ml"
                
              in
              let l =
                let xs = xs0 in
                
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 6249 "parser.ml"
                
              in
              
# 138 "parser.mly"
       ( RFun (t :: l, ret) )
# 6255 "parser.ml"
              
            in
            let _startpos_r_ = _startpos__10_ in
            let _endpos = _endpos__2_ in
            let _startpos = _startpos_r_ in
            
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 6264 "parser.ml"
             in
            _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
        | QUESTION ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState320 | MenhirState376 | MenhirState371 | MenhirState331 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState358 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NULL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos__2_ = _endpos in
                    let ((_menhir_stack, _menhir_s, (t0 : (
# 94 "parser.mly"
      (Ast.ty)
# 6302 "parser.ml"
                    )), _startpos_t0_), _endpos__30_, _) = _menhir_stack in
                    let _2 = () in
                    let _30 = () in
                    let _20 = () in
                    let _v : (Ast.exp Ast.node) = let r =
                      let _3 = _30 in
                      let _2 = _20 in
                      let t = t0 in
                      
# 133 "parser.mly"
                           ( RArray t )
# 6314 "parser.ml"
                      
                    in
                    let _startpos_r_ = _startpos_t0_ in
                    let _endpos = _endpos__2_ in
                    let _startpos = _startpos_r_ in
                    
# 164 "parser.mly"
               ( loc _startpos _endpos @@ CNull r )
# 6323 "parser.ml"
                     in
                    _menhir_goto_gexp _menhir_env _menhir_stack _menhir_s _v
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState358)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState369 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run355 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INT _v ->
                        _menhir_run354 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run332 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | NEW ->
                        _menhir_run327 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | STRING _v ->
                        _menhir_run326 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TINT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TRUE ->
                        _menhir_run325 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TSTRING ->
                        _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | UIDENT _v ->
                        _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | RBRACE ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState371 in
                        let _v : (Ast.exp Ast.node list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 6397 "parser.ml"
                         in
                        _menhir_goto_loption_separated_nonempty_list_COMMA_gexp__ _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState371)
                | QUESTION ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
                | LBRACKET ->
                    _menhir_reduce138 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState369)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState389 | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState382 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState382 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TINT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState382 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TSTRING ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState382 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState382 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAREN ->
                    _menhir_reduce94 _menhir_env (Obj.magic _menhir_stack) MenhirState382
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState382)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 6480 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 6491 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.stmt Ast.node option) = 
# 116 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 6496 "parser.ml"
         in
        _menhir_goto_option_stmt_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState302 | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IFQ ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RETURN ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | VAR ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHILE ->
            _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState302
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302)
    | MenhirState391 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 6563 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 88 "parser.mly"
      (Ast.stmt Ast.node)
# 6569 "parser.ml"
            ) = 
# 101 "parser.mly"
               ( s )
# 6573 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 88 "parser.mly"
      (Ast.stmt Ast.node)
# 6580 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMI_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.id * Ast.exp Ast.node) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__5_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (
# 15 "parser.mly"
       (string)
# 6608 "parser.ml"
        )), _startpos_t_), _, (xs0 : ((Ast.id * Ast.exp Ast.node) list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 6618 "parser.ml"
        ) = let cs =
          let xs = xs0 in
          
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 6624 "parser.ml"
          
        in
        let _endpos = _endpos__5_ in
        let _startpos = _startpos__1_ in
        
# 193 "parser.mly"
                        ( loc _startpos _endpos @@ CStruct(t, cs) )
# 6632 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 6645 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce139 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 15 "parser.mly"
       (string)
# 6701 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id0 : (
# 15 "parser.mly"
       (string)
# 6707 "parser.ml"
    )), _startpos_id0_) = _menhir_stack in
    let _startpos = _startpos_id0_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6713 "parser.ml"
    ) = let r =
      let id = id0 in
      
# 134 "parser.mly"
              ( RStruct id )
# 6719 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 6725 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 15 "parser.mly"
       (string)
# 6732 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (id0 : (
# 15 "parser.mly"
       (string)
# 6740 "parser.ml"
    )), _startpos_id0_) = _menhir_stack in
    let _2 = () in
    let _startpos = _startpos_id0_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6747 "parser.ml"
    ) = let r =
      let id = id0 in
      
# 134 "parser.mly"
              ( RStruct id )
# 6753 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 6759 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce137 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _startpos__10_) = _menhir_stack in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6771 "parser.ml"
    ) = let r =
      let _1 = _10 in
      
# 132 "parser.mly"
            ( RString )
# 6777 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 6783 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _startpos__10_) = _menhir_stack in
    let _2 = () in
    let _10 = () in
    let _startpos = _startpos__10_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6798 "parser.ml"
    ) = let r =
      let _1 = _10 in
      
# 132 "parser.mly"
            ( RString )
# 6804 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 6810 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce140 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, _startpos__11_), _endpos__20_, _), _, _startpos__100_) = _menhir_stack in
    let _100 = () in
    let _30 = () in
    let _20 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6825 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _3 = _30 in
      let _2 = _20 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 6836 "parser.ml"
        
      in
      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 6842 "parser.ml"
      
    in
    
# 122 "parser.mly"
           ( TRef r )
# 6848 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run22 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * Lexing.position * _menhir_state)) * _menhir_state * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (((_menhir_stack, _menhir_s, _startpos__11_), _endpos__20_, _), _, _startpos__100_) = _menhir_stack in
    let _2 = () in
    let _100 = () in
    let _30 = () in
    let _20 = () in
    let _11 = () in
    let _startpos = _startpos__11_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 6866 "parser.ml"
    ) = let r =
      let _10 = _100 in
      let _3 = _30 in
      let _2 = _20 in
      let _1 = _11 in
      let ret =
        let _1 = _10 in
        
# 128 "parser.mly"
           ( RetVoid )
# 6877 "parser.ml"
        
      in
      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 6883 "parser.ml"
      
    in
    
# 123 "parser.mly"
                    ( TNullRef r )
# 6889 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 14 "parser.mly"
       (string)
# 6896 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 14 "parser.mly"
       (string)
# 6902 "parser.ml"
    )), _startpos_id_) = _menhir_stack in
    let _startpos = _startpos_id_ in
    let _endpos = _endpos_id_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 6909 "parser.ml"
    ) = let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    
# 197 "parser.mly"
                        ( loc _startpos _endpos @@ Id id )
# 6915 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_lhs : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp Ast.node) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState287)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_vdecl__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.vdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState277 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState277 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState277 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState277 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState277 in
            let _v : (Ast.exp Ast.node option) = 
# 114 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( None )
# 7022 "parser.ml"
             in
            _menhir_goto_option_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState277)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7039 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPER | BANGEQ | BAR | COMMA | DASH | DOT | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7058 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7066 "parser.ml"
            ) = let u =
              let _1 = _10 in
              
# 160 "parser.mly"
          ( Lognot )
# 7072 "parser.ml"
              
            in
            let _startpos_u_ = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 203 "parser.mly"
                        ( loc _startpos _endpos @@ Uop (u, e) )
# 7081 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState292 | MenhirState143 | MenhirState109 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DASH ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LENGTH ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TILDE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7177 "parser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : (Ast.exp Ast.node list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 7182 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPER | BANGEQ | BAR | COMMA | DASH | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7207 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7211 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7219 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 143 "parser.mly"
           ( Mul )
# 7225 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7233 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | DASH | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7315 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7319 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7327 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 141 "parser.mly"
           ( Add )
# 7333 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7341 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7372 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7376 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7384 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 154 "parser.mly"
           ( Shl )
# 7390 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7398 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | DASH | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7425 "parser.ml"
            )), _startpos_e1_), _startpos__10_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7429 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7437 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 142 "parser.mly"
           ( Sub )
# 7443 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7451 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | IAND | IOR | LT | LTEQ | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7488 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7492 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7500 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 147 "parser.mly"
           ( Lte )
# 7506 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7514 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7545 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7549 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7557 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 156 "parser.mly"
           ( Sar )
# 7563 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7571 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7602 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7606 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7614 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 155 "parser.mly"
           ( Shr )
# 7620 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7628 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | IAND | IOR | LT | LTEQ | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7665 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7669 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7677 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 146 "parser.mly"
           ( Lt )
# 7683 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7691 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EOF | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7746 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7750 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7758 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 153 "parser.mly"
           ( IOr )
# 7764 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7772 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EOF | IAND | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7825 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7829 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7837 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 152 "parser.mly"
           ( IAnd )
# 7843 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7851 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | IAND | IOR | LT | LTEQ | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7888 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7892 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7900 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 149 "parser.mly"
           ( Gte )
# 7906 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7914 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | GT | GTEQ | IAND | IOR | LT | LTEQ | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7951 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7955 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 7963 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 148 "parser.mly"
           ( Gt )
# 7969 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 7977 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | IAND | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8022 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8026 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8034 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 144 "parser.mly"
           ( Eq )
# 8040 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 8048 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | EOF | IAND | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8099 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8103 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8111 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 151 "parser.mly"
           ( Or )
# 8117 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 8125 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | EOF | EQEQ | IAND | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8170 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8174 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8182 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 145 "parser.mly"
           ( Neq )
# 8188 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 8196 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BAR | COMMA | EOF | IAND | IOR | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8245 "parser.ml"
            )), _startpos_e1_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8249 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8257 "parser.ml"
            ) = let b =
              let _1 = _10 in
              
# 150 "parser.mly"
           ( And )
# 8263 "parser.ml"
              
            in
            let _endpos = _endpos_e2_ in
            let _startpos = _startpos_e1_ in
            
# 202 "parser.mly"
                        ( loc _startpos _endpos @@ Bop (b, e1, e2) )
# 8271 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | AMPER | BANGEQ | BAR | COMMA | DASH | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8298 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8306 "parser.ml"
            ) = let u =
              let _1 = _10 in
              
# 159 "parser.mly"
          ( Neg )
# 8312 "parser.ml"
              
            in
            let _startpos_u_ = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 203 "parser.mly"
                        ( loc _startpos _endpos @@ Uop (u, e) )
# 8321 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8380 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8390 "parser.ml"
            ) = let _endpos = _endpos__4_ in
            let _startpos = _startpos__1_ in
            
# 205 "parser.mly"
                        ( loc _startpos _endpos @@ Length(e) )
# 8396 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8457 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8466 "parser.ml"
            ) = 
# 206 "parser.mly"
                        ( e )
# 8470 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 14 "parser.mly"
       (string)
# 8529 "parser.ml"
            )), _startpos_id_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8533 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.id * Ast.exp Ast.node) = 
# 209 "parser.mly"
                      ( (id, e) )
# 8539 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.id * Ast.exp Ast.node))) = _menhir_stack in
                let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 8565 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMI_field_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ARROW ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BANG ->
                            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | DASH ->
                            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | FALSE ->
                            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | IDENT _v ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | INT _v ->
                            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LENGTH ->
                            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | LPAREN ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | NEW ->
                            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | STRING _v ->
                            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TBOOL ->
                            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TILDE ->
                            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TINT ->
                            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TRUE ->
                            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | TSTRING ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | UIDENT _v ->
                            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _, _menhir_s, _, _), _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__10_ = _endpos in
            let ((((((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (
# 94 "parser.mly"
      (Ast.ty)
# 8756 "parser.ml"
            )), _startpos_t_), _endpos_e1_, _, (e1 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8760 "parser.ml"
            )), _startpos_e1_), _endpos__5_), _endpos_u_, (u : (
# 14 "parser.mly"
       (string)
# 8764 "parser.ml"
            )), _startpos_u_), _endpos_e2_, _, (e2 : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8768 "parser.ml"
            )), _startpos_e2_) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__10_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8781 "parser.ml"
            ) = let _endpos = _endpos__10_ in
            let _startpos = _startpos__1_ in
            
# 196 "parser.mly"
                        ( loc _startpos _endpos @@ NewArr(t, e1, u, e2) )
# 8787 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AMPER | BANGEQ | BAR | COMMA | DASH | DOT | EOF | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LT | LTEQ | LTLT | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__10_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8812 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _10 = () in
            let _startpos = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8820 "parser.ml"
            ) = let u =
              let _1 = _10 in
              
# 161 "parser.mly"
          ( Bitnot )
# 8826 "parser.ml"
              
            in
            let _startpos_u_ = _startpos__10_ in
            let _endpos = _endpos_e_ in
            let _startpos = _startpos_u_ in
            
# 203 "parser.mly"
                        ( loc _startpos _endpos @@ Uop (u, e) )
# 8835 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 8865 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _v : (
# 87 "parser.mly"
      (Ast.exp Ast.node)
# 8871 "parser.ml"
            ) = 
# 98 "parser.mly"
              ( e )
# 8875 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 87 "parser.mly"
      (Ast.exp Ast.node)
# 8882 "parser.ml"
            )) = _v in
            Obj.magic _1
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 9029 "parser.ml"
            )), _startpos_id_), _endpos_init_, _, (init : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 9033 "parser.ml"
            )), _startpos_init_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _v : (Ast.vdecl) = 
# 212 "parser.mly"
                             ( (id, init) )
# 9041 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
            (match _menhir_s with
            | MenhirState273 | MenhirState271 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | VAR ->
                        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273)
                | SEMI ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (Ast.vdecl)), _startpos_x_) = _menhir_stack in
                    let _v : (Ast.vdecl list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 9067 "parser.ml"
                     in
                    _menhir_goto_separated_nonempty_list_COMMA_vdecl_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState391 | MenhirState302 | MenhirState169 | MenhirState279 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos__2_ = _endpos in
                    let (_menhir_stack, _menhir_s, (d : (Ast.vdecl)), _startpos_d_) = _menhir_stack in
                    let _2 = () in
                    let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 9092 "parser.ml"
                    ) = let _endpos = _endpos__2_ in
                    let _startpos = _startpos_d_ in
                    
# 215 "parser.mly"
                        ( loc _startpos _endpos @@ Decl(d) )
# 9098 "parser.ml"
                     in
                    _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 9165 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 9172 "parser.ml"
            ) = let _endpos = _endpos__3_ in
            let _startpos = _startpos__1_ in
            
# 221 "parser.mly"
                        ( loc _startpos _endpos @@ Ret(Some e) )
# 9178 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState189
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState195
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState205
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState215
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState222
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState232
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState239)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState257
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState257)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState287 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let ((_menhir_stack, _menhir_s, (p : (Ast.exp Ast.node)), _startpos_p_), _endpos_e_, _, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 9859 "parser.ml"
            )), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 9866 "parser.ml"
            ) = let _endpos = _endpos__4_ in
            let _startpos = _startpos_p_ in
            
# 216 "parser.mly"
                        ( loc _startpos _endpos @@ Assn(p,e) )
# 9872 "parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState391 | MenhirState169 | MenhirState302 | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 9914 "parser.ml"
                    )), _startpos_e_), _endpos_id_, (id : (
# 14 "parser.mly"
       (string)
# 9918 "parser.ml"
                    )), _startpos_id_) = _menhir_stack in
                    let _2 = () in
                    let _startpos = _startpos_e_ in
                    let _v : (Ast.exp Ast.node) = let _endpos = _endpos_id_ in
                    let _startpos = _startpos_e_ in
                    
# 182 "parser.mly"
                        ( loc _startpos _endpos @@ Proj (e, id) )
# 9927 "parser.ml"
                     in
                    _menhir_goto_lhs _menhir_env _menhir_stack _menhir_s _v _startpos
                | AMPER | BANGEQ | BAR | DASH | DOT | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LBRACKET | LPAREN | LT | LTEQ | LTLT | PLUS | STAR ->
                    _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _, _menhir_s, _, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DASH ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState296 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState296 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LENGTH ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState296 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TILDE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState296)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DASH ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState292 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState292 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LENGTH ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState292 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TILDE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState292 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState292
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState292)
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 10109 "parser.ml"
                )), _startpos_e_), _endpos_i_, _, (i : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 10113 "parser.ml"
                )), _startpos_i_), _endpos__4_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _startpos = _startpos_e_ in
                let _v : (Ast.exp Ast.node) = let _endpos = _endpos__4_ in
                let _startpos = _startpos_e_ in
                
# 181 "parser.mly"
                        ( loc _startpos _endpos @@ Index (e, i) )
# 10123 "parser.ml"
                 in
                _menhir_goto_lhs _menhir_env _menhir_stack _menhir_s _v _startpos
            | AMPER | BANGEQ | BAR | DASH | DOT | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LBRACKET | LPAREN | LT | LTEQ | LTLT | PLUS | STAR ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState277 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AMPER ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | BANGEQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | BAR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | DASH ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EQEQ ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | GTEQ ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | GTGT ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GTGTGT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | IAND ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | IOR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | LTEQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LTLT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 10190 "parser.ml"
            )), _startpos_x_) = _menhir_stack in
            let _v : (Ast.exp Ast.node option) = 
# 116 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( Some x )
# 10195 "parser.ml"
             in
            _menhir_goto_option_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run58 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 10210 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.prog) = 
# 199 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 10222 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 10229 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | QUESTION ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
        _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce94 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | QUESTION ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
        _menhir_reduce137 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run307 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState309 in
                let _v : (Ast.field list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 10348 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMI_decl_field__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState309)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState25 in
        let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TVOID ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState27 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | QUESTION ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
                | COMMA | IDENT _ | LBRACKET | NULL | RPAREN ->
                    _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run318 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run355 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run354 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run332 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run327 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run326 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run325 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run323 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run321 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState320)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState391 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState389 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState384 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState382 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState376 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState371 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState369 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState366 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState358 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState349 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState346 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState334 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState332 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState331 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState292 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState287 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState277 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState271 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState257 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState245 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState233 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _, _menhir_s, _, _), _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run175 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DASH ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FALSE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LENGTH ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NEW ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TILDE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRUE ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 11106 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NULL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, (id0 : (
# 15 "parser.mly"
       (string)
# 11122 "parser.ml"
        )), _startpos_id0_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_id0_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11130 "parser.ml"
        ) = let r =
          let id = id0 in
          
# 134 "parser.mly"
              ( RStruct id )
# 11136 "parser.ml"
          
        in
        let _startpos_r_ = _startpos_id0_ in
        let _endpos = _endpos__2_ in
        let _startpos = _startpos_r_ in
        
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 11145 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | QUESTION ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | LBRACKET | RPAREN ->
        _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NULL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__10_) = _menhir_stack in
        let _2 = () in
        let _10 = () in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11179 "parser.ml"
        ) = let r =
          let _1 = _10 in
          
# 132 "parser.mly"
            ( RString )
# 11185 "parser.ml"
          
        in
        let _startpos_r_ = _startpos__10_ in
        let _endpos = _endpos__2_ in
        let _startpos = _startpos_r_ in
        
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 11194 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | QUESTION ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | LBRACKET | RPAREN ->
        _menhir_reduce137 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11220 "parser.ml"
    ) = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    
# 186 "parser.mly"
                        ( loc _startpos _endpos @@ CBool true )
# 11226 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 11240 "parser.ml"
    ) = 
# 121 "parser.mly"
           ( TInt )
# 11244 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (
# 94 "parser.mly"
      (Ast.ty)
# 11299 "parser.ml"
    ) = 
# 125 "parser.mly"
           ( TBool )
# 11303 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 13 "parser.mly"
       (string)
# 11310 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (
# 13 "parser.mly"
       (string)
# 11319 "parser.ml"
    )) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11327 "parser.ml"
    ) = let _endpos = _endpos_s_ in
    let _startpos = _startpos_s_ in
    
# 189 "parser.mly"
                        ( loc _startpos _endpos @@ CStr s )
# 11333 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run179 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState179 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (
# 92 "parser.mly"
      (Ast.stmt Ast.node)
# 11372 "parser.ml"
        ) = let _endpos = _endpos__2_ in
        let _startpos = _startpos__1_ in
        
# 220 "parser.mly"
                        ( loc _startpos _endpos @@ Ret(None) )
# 11378 "parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _menhir_s _v
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState12 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState14 in
                let _v : ((Ast.id * Ast.exp Ast.node) list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 11435 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_SEMI_field__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
        | QUESTION ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
        | LBRACKET ->
            _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_s = MenhirState17 in
        let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TVOID ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState19 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NULL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos__2_ = _endpos in
                    let (((_menhir_stack, _menhir_s, _startpos__11_), _endpos__20_, _), _, _startpos__100_) = _menhir_stack in
                    let _2 = () in
                    let _100 = () in
                    let _30 = () in
                    let _20 = () in
                    let _11 = () in
                    let _startpos = _startpos__11_ in
                    let _endpos = _endpos__2_ in
                    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11525 "parser.ml"
                    ) = let r =
                      let _10 = _100 in
                      let _3 = _30 in
                      let _2 = _20 in
                      let _1 = _11 in
                      let ret =
                        let _1 = _10 in
                        
# 128 "parser.mly"
           ( RetVoid )
# 11536 "parser.ml"
                        
                      in
                      
# 135 "parser.mly"
                                   ( RFun ([], ret) )
# 11542 "parser.ml"
                      
                    in
                    let _startpos_r_ = _startpos__11_ in
                    let _endpos = _endpos__2_ in
                    let _startpos = _startpos_r_ in
                    
# 185 "parser.mly"
                        ( loc _startpos _endpos @@ CNull r )
# 11551 "parser.ml"
                     in
                    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                | QUESTION ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
                | COMMA | LBRACKET | RPAREN ->
                    _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run57 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 11 "parser.mly"
       (int64)
# 11652 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_i_ = _endpos in
    let (i : (
# 11 "parser.mly"
       (int64)
# 11661 "parser.ml"
    )) = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 11669 "parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 188 "parser.mly"
                        ( loc _startpos _endpos @@ CInt i )
# 11675 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run183 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState184 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState208 in
                let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ARROW ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LPAREN ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TINT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TSTRING ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TVOID ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState210 in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | IDENT _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | EQ ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | BANG ->
                                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | DASH ->
                                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | FALSE ->
                                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | IDENT _v ->
                                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | INT _v ->
                                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | LENGTH ->
                                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | LPAREN ->
                                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | NEW ->
                                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | STRING _v ->
                                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | TBOOL ->
                                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | TILDE ->
                                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | TINT ->
                                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | TRUE ->
                                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | TSTRING ->
                                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | UIDENT _v ->
                                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | QUESTION ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
                        | LBRACKET ->
                            _menhir_reduce140 _menhir_env (Obj.magic _menhir_stack)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | UIDENT _v ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TINT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TSTRING ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState184 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BANG ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | DASH ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | FALSE ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INT _v ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LENGTH ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | NEW ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | STRING _v ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TILDE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TINT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TRUE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TSTRING ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | UIDENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | QUESTION ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
            | LBRACKET ->
                _menhir_reduce137 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | UIDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState184 in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EQ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BANG ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | DASH ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | FALSE ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | INT _v ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LENGTH ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | NEW ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | STRING _v ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TILDE ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TINT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TRUE ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TSTRING ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | UIDENT _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | QUESTION ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
            | LBRACKET ->
                _menhir_reduce139 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run192 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DASH ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FALSE ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LENGTH ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NEW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TBOOL ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TILDE ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TINT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRUE ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TSTRING ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run269 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 12032 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_id_, _menhir_s, (id : (
# 14 "parser.mly"
       (string)
# 12044 "parser.ml"
        )), _startpos_id_) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _v : (Ast.exp Ast.node) = let _endpos = _endpos_id_ in
        let _startpos = _startpos_id_ in
        
# 179 "parser.mly"
                        ( loc _startpos _endpos @@ Id id )
# 12052 "parser.ml"
         in
        _menhir_goto_lhs _menhir_env _menhir_stack _menhir_s _v _startpos
    | AMPER | BANGEQ | BAR | DASH | DOT | EQEQ | GT | GTEQ | GTGT | GTGTGT | IAND | IOR | LBRACKET | LPAREN | LT | LTEQ | LTLT | PLUS | STAR ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run270 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState271 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState271 in
            let _v : (Ast.vdecl list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 12085 "parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_vdecl__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState271)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run59 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (
# 91 "parser.mly"
      (Ast.exp Ast.node)
# 12111 "parser.ml"
    ) = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    
# 187 "parser.mly"
                        ( loc _startpos _endpos @@ CBool false )
# 12117 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and exp_top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 87 "parser.mly"
      (Ast.exp Ast.node)
# 12228 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 90 "parser.mly"
      (Ast.prog)
# 12274 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GLOBAL ->
        _menhir_run318 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRUCT ->
        _menhir_run307 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TVOID ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState157
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)

and stmt_top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 88 "parser.mly"
      (Ast.stmt Ast.node)
# 12308 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DASH ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FALSE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run270 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState391 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run192 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IFQ ->
        _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState391 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LENGTH ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NEW ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RETURN ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState391 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TILDE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TINT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRUE ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TSTRING ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | VAR ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHILE ->
        _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState391)

# 233 "/home/dominik/.opam/system/lib/menhir/standard.mly"
  

# 12366 "parser.ml"
