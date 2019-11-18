
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | VOID
    | UID of (
# 63 "ll/llparser.mly"
       (string)
# 13 "ll/llparser.ml"
  )
    | TYPE
    | TO
    | SUB
    | STRING of (
# 64 "ll/llparser.mly"
       (string)
# 21 "ll/llparser.ml"
  )
    | STORE
    | STAR
    | SLT
    | SLE
    | SHL
    | SGT
    | SGE
    | RPAREN
    | RET
    | RBRACKET
    | RBRACE
    | OR
    | NULL
    | NE
    | MUL
    | LSHR
    | LPAREN
    | LOAD
    | LBRACKET
    | LBRACE
    | LBL of (
# 61 "ll/llparser.mly"
       (string)
# 46 "ll/llparser.ml"
  )
    | LABEL
    | INT of (
# 60 "ll/llparser.mly"
       (int)
# 52 "ll/llparser.ml"
  )
    | ICMP
    | I8
    | I64
    | I32
    | I1
    | GLOBAL
    | GID of (
# 62 "ll/llparser.mly"
       (string)
# 63 "ll/llparser.ml"
  )
    | GEP
    | EXTERNAL
    | EQUALS
    | EQ
    | EOF
    | ENTRY
    | DEFINE
    | DECLARE
    | CROSS
    | COMMA
    | COLON
    | CALL
    | BR
    | BITCAST
    | ASHR
    | AND
    | ALLOCA
    | ADD
  
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
  | MenhirState186
  | MenhirState185
  | MenhirState184
  | MenhirState182
  | MenhirState181
  | MenhirState175
  | MenhirState169
  | MenhirState160
  | MenhirState155
  | MenhirState154
  | MenhirState149
  | MenhirState147
  | MenhirState146
  | MenhirState145
  | MenhirState142
  | MenhirState141
  | MenhirState139
  | MenhirState138
  | MenhirState136
  | MenhirState134
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState117
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState111
  | MenhirState108
  | MenhirState106
  | MenhirState102
  | MenhirState100
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState89
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState64
  | MenhirState60
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState40
  | MenhirState39
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState32
  | MenhirState31
  | MenhirState28
  | MenhirState26
  | MenhirState22
  | MenhirState19
  | MenhirState17
  | MenhirState16
  | MenhirState11
  | MenhirState10
  | MenhirState5

# 1 "ll/llparser.mly"
   open Ll
   open Llutil.Parsing


# 177 "ll/llparser.ml"

let rec _menhir_goto_ty_operand_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.ty * Ll.operand) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) = 
# 181 "ll/llparser.mly"
    ( List.rev tos )
# 215 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState114 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 231 "ll/llparser.ml"
                ))), _, (t : (Ll.ty))), _, (o : (Ll.operand))), _, (args : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ll.uid * Ll.insn) = 
# 262 "ll/llparser.mly"
    ( (u, Call (t, o, args)) )
# 240 "ll/llparser.ml"
                 in
                _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState149 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (t : (Ll.ty))), _, (o : (Ll.operand))), _, (args : ((Ll.ty * Ll.operand) list))) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : (Ll.uid * Ll.insn) = 
# 260 "ll/llparser.mly"
    ( (gensym "call", Call (t, o, args)) )
# 265 "ll/llparser.ml"
                 in
                _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.ty * Ll.operand) list) = 
# 173 "ll/llparser.mly"
    ( [] )
# 288 "ll/llparser.ml"
     in
    _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_i_operand_list_rev : _menhir_env -> 'ttv_tail -> (Ll.operand list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I32 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | I64 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | BR | CALL | RET | STORE | UID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (os : (Ll.operand list))) = _menhir_stack in
        let _v : (Ll.operand list) = 
# 197 "ll/llparser.mly"
    ( List.rev os )
# 349 "ll/llparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (os : (Ll.operand list)) = _v in
        let (((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 357 "ll/llparser.ml"
        ))), _, (_4 : (Ll.ty))), _), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _8 = () in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) = 
# 266 "ll/llparser.mly"
    ( (u, Gep (t,o,os)) )
# 366 "ll/llparser.ml"
         in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_list_insn_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.uid * Ll.insn) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState175 | MenhirState64 | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (is : ((Ll.uid * Ll.insn) list)) = _v in
        let _v : ((Ll.uid * Ll.insn) list) = 
# 270 "ll/llparser.mly"
    ( is )
# 385 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | UID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (l : (
# 63 "ll/llparser.mly"
       (string)
# 426 "ll/llparser.ml"
                    )) = _v in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ll.terminator) = 
# 205 "ll/llparser.mly"
    ( Br l )
# 433 "ll/llparser.ml"
                     in
                    _menhir_goto_terminator _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | RET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ll.uid * Ll.insn) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ll.uid * Ll.insn))) = _menhir_stack in
        let _v : ((Ll.uid * Ll.insn) list) = 
# 201 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 483 "ll/llparser.ml"
         in
        _menhir_goto_list_insn_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cnd : _menhir_env -> 'ttv_tail -> (Ll.cnd) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_goto_bop : _menhir_env -> 'ttv_tail -> (Ll.bop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_goto_gdecl_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.ty * Ll.ginit) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | RBRACE | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (gs : ((Ll.ty * Ll.ginit) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.ginit) list) = 
# 282 "ll/llparser.mly"
    ( List.rev gs )
# 577 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState37 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (gs : ((Ll.ty * Ll.ginit) list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ginit) = 
# 296 "ll/llparser.mly"
    ( GStruct gs )
# 596 "ll/llparser.ml"
                 in
                _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState35 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (gs : ((Ll.ty * Ll.ginit) list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ginit) = 
# 294 "ll/llparser.mly"
    ( GArray gs )
# 620 "ll/llparser.ml"
                 in
                _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_block_list_rev : _menhir_env -> 'ttv_tail -> ((Ll.lbl * Ll.block) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | STORE ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | UID _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
            | BR | RET ->
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState175
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (bs : ((Ll.lbl * Ll.block) list))) = _menhir_stack in
        let _v : ((Ll.lbl * Ll.block) list) = 
# 221 "ll/llparser.mly"
    ( List.rev bs )
# 679 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _, (t : (Ll.ty))), _, (l : (
# 62 "ll/llparser.mly"
       (string)
# 693 "ll/llparser.ml"
            ))), _, (a : ((Ll.ty * Ll.uid) list))), _, (eb : (Ll.block))), (bs : ((Ll.lbl * Ll.block) list))) = _menhir_stack in
            let _10 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ll.gid * Ll.fdecl) = 
# 96 "ll/llparser.mly"
    ( (l, { f_ty = (List.map fst a, t)
          ; f_param = List.map snd a
          ; f_cfg = (eb, bs)
          }
    ) )
# 707 "ll/llparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (f : (Ll.gid * Ll.fdecl)) = _v in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _v : (Ll.prog) = 
# 85 "ll/llparser.mly"
    ( { ds with fdecls = f :: ds.fdecls }  )
# 716 "ll/llparser.ml"
             in
            _menhir_goto_decls_rev _menhir_env _menhir_stack _v
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
        raise _eRR

and _menhir_goto_entry_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : ((Ll.lbl * Ll.block) list) = 
# 215 "ll/llparser.mly"
    ( [] )
# 739 "ll/llparser.ml"
     in
    _menhir_goto_block_list_rev _menhir_env _menhir_stack _v

and _menhir_goto_operand : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.operand) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 753 "ll/llparser.ml"
        ))), _, (_4 : (Ll.ty))), _), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) = 
# 254 "ll/llparser.mly"
    ( (u, Load (t,o)) )
# 761 "ll/llparser.ml"
         in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 798 "ll/llparser.ml"
        ))), (c : (Ll.cnd))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) = 
# 258 "ll/llparser.mly"
    ( (u, Icmp (c,t,o1,o2)) )
# 806 "ll/llparser.ml"
         in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I32 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
            | I64 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GID _v ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | INT _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | NULL ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | UID _v ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
            | BR | CALL | COMMA | RET | STORE | UID _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (Ll.operand list) = 
# 185 "ll/llparser.mly"
    ( [] )
# 858 "ll/llparser.ml"
                 in
                _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
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
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) = 
# 187 "ll/llparser.mly"
      ( [o] )
# 881 "ll/llparser.ml"
         in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.operand list) = 
# 189 "ll/llparser.mly"
    ( [o] )
# 892 "ll/llparser.ml"
         in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) = 
# 191 "ll/llparser.mly"
    ( o::os )
# 904 "ll/llparser.ml"
         in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (os : (Ll.operand list))), _, (o : (Ll.operand))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.operand list) = 
# 193 "ll/llparser.mly"
    ( o::os )
# 916 "ll/llparser.ml"
         in
        _menhir_goto_i_operand_list_rev _menhir_env _menhir_stack _v
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | COMMA | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (tos : ((Ll.ty * Ll.operand) list))), _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ll.ty * Ll.operand) list) = 
# 177 "ll/llparser.mly"
    ( (t,o)::tos )
# 963 "ll/llparser.ml"
         in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.operand) list) = 
# 175 "ll/llparser.mly"
    ( [(t,o)] )
# 973 "ll/llparser.ml"
         in
        _menhir_goto_ty_operand_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | GID _v ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | INT _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | NULL ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | UID _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 1044 "ll/llparser.ml"
        ))), (b : (Ll.bop))), _, (t : (Ll.ty))), _, (o1 : (Ll.operand))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _6 = () in
        let _2 = () in
        let _v : (Ll.uid * Ll.insn) = 
# 250 "ll/llparser.mly"
    ( (u, Binop (b,t,o1,o2)) )
# 1051 "ll/llparser.ml"
         in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (t1 : (Ll.ty))), _, (o1 : (Ll.operand))), _, (t2 : (Ll.ty))), _, (o2 : (Ll.operand))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ll.uid * Ll.insn) = 
# 256 "ll/llparser.mly"
    ( (gensym "store", Store (t1,o1,o2)) )
# 1097 "ll/llparser.ml"
         in
        _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | COMMA | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (t : (Ll.ty))), _, (o : (Ll.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (Ll.terminator) = 
# 201 "ll/llparser.mly"
    ( Ret (t, Some o) )
# 1144 "ll/llparser.ml"
         in
        _menhir_goto_terminator _menhir_env _menhir_stack _v
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | UID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LABEL ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | UID _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (l2 : (
# 63 "ll/llparser.mly"
       (string)
# 1185 "ll/llparser.ml"
                                )) = _v in
                                let ((_menhir_stack, _, (o : (Ll.operand))), (l1 : (
# 63 "ll/llparser.mly"
       (string)
# 1190 "ll/llparser.ml"
                                ))) = _menhir_stack in
                                let _8 = () in
                                let _7 = () in
                                let _5 = () in
                                let _4 = () in
                                let _2 = () in
                                let _1 = () in
                                let _v : (Ll.terminator) = 
# 207 "ll/llparser.mly"
    ( Cbr (o,l1,l2) )
# 1201 "ll/llparser.ml"
                                 in
                                _menhir_goto_terminator _menhir_env _menhir_stack _v
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
                            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.uid * Ll.insn) list) = 
# 199 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 1248 "ll/llparser.ml"
     in
    _menhir_goto_list_insn_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "ll/llparser.mly"
       (string)
# 1255 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 231 "ll/llparser.mly"
        ( Add )
# 1275 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | ALLOCA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 236 "ll/llparser.mly"
        ( And )
# 1309 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | ASHR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 238 "ll/llparser.mly"
         ( Ashr )
# 1320 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | BITCAST ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | CALL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | GEP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | ICMP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 241 "ll/llparser.mly"
       ( Eq )
# 1405 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | NE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 242 "ll/llparser.mly"
       ( Ne )
# 1416 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SGE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 246 "ll/llparser.mly"
        ( Sge )
# 1427 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SGT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 245 "ll/llparser.mly"
        ( Sgt )
# 1438 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SLE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 244 "ll/llparser.mly"
        ( Sle )
# 1449 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | SLT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (Ll.cnd) = 
# 243 "ll/llparser.mly"
        ( Slt )
# 1460 "ll/llparser.ml"
                 in
                _menhir_goto_cnd _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LOAD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | LSHR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 237 "ll/llparser.mly"
         ( Lshr )
# 1500 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | MUL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 233 "ll/llparser.mly"
        ( Mul )
# 1511 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 230 "ll/llparser.mly"
       ( Or )
# 1522 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | SHL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 234 "ll/llparser.mly"
        ( Shl )
# 1533 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 232 "ll/llparser.mly"
        ( Sub )
# 1544 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
        | XOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (Ll.bop) = 
# 235 "ll/llparser.mly"
        ( Xor )
# 1555 "ll/llparser.ml"
             in
            _menhir_goto_bop _menhir_env _menhir_stack _v
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Ll.ty * Ll.ginit) list) = 
# 274 "ll/llparser.mly"
    ( [] )
# 1626 "ll/llparser.ml"
     in
    _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ginit : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ginit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.ginit)) = _v in
        let ((_menhir_stack, _menhir_s, (gs : ((Ll.ty * Ll.ginit) list))), _, (t : (Ll.ty))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ll.ty * Ll.ginit) list) = 
# 278 "ll/llparser.mly"
    ( (t,g)::gs )
# 1642 "ll/llparser.ml"
         in
        _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.ginit)) = _v in
        let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.ginit) list) = 
# 276 "ll/llparser.mly"
    ( [(t,g)] )
# 1653 "ll/llparser.ml"
         in
        _menhir_goto_gdecl_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (gi : (Ll.ginit)) = _v in
        let ((_menhir_stack, (g : (
# 62 "ll/llparser.mly"
       (string)
# 1663 "ll/llparser.ml"
        ))), _, (t : (Ll.ty))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (Ll.gid * Ll.gdecl) = 
# 104 "ll/llparser.mly"
    ( (g, (t,gi)) )
# 1670 "ll/llparser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (g : (Ll.gid * Ll.gdecl)) = _v in
        let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
        let _v : (Ll.prog) = 
# 87 "ll/llparser.mly"
    ( { ds with gdecls = g :: ds.gdecls }  )
# 1679 "ll/llparser.ml"
         in
        _menhir_goto_decls_rev _menhir_env _menhir_stack _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_ty_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ll.ty list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ll.ty))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ll.ty list) = 
# 231 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 1697 "ll/llparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ll.ty list)) = _v in
        let _v : (Ll.ty list) = 
# 144 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( x )
# 1707 "ll/llparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _, (rt : (Ll.ty))), _, (g : (
# 62 "ll/llparser.mly"
       (string)
# 1727 "ll/llparser.ml"
        ))), _, (xs0 : (Ll.ty list))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (Ll.gid * Ll.ty) = let ts =
          let xs = xs0 in
          
# 220 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 1737 "ll/llparser.ml"
          
        in
        
# 112 "ll/llparser.mly"
    ( (g, Fun (ts,rt)) )
# 1743 "ll/llparser.ml"
         in
        _menhir_goto_edecl _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_terminator : _menhir_env -> 'ttv_tail -> (Ll.terminator) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ll.terminator)) = _v in
    let (_menhir_stack, _menhir_s, (is : ((Ll.uid * Ll.insn) list))) = _menhir_stack in
    let _v : (Ll.block) = 
# 211 "ll/llparser.mly"
    ( { insns = is; term=(gensym "tmn", t) }  )
# 1762 "ll/llparser.ml"
     in
    match _menhir_s with
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ll.block) = 
# 225 "ll/llparser.mly"
    ( b )
# 1775 "ll/llparser.ml"
         in
        _menhir_goto_entry_block _menhir_env _menhir_stack _menhir_s _v
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let ((_menhir_stack, (bs : ((Ll.lbl * Ll.block) list))), (l : (
# 61 "ll/llparser.mly"
       (string)
# 1785 "ll/llparser.ml"
        ))) = _menhir_stack in
        let _3 = () in
        let _v : ((Ll.lbl * Ll.block) list) = 
# 217 "ll/llparser.mly"
    ( (l,b) :: bs )
# 1791 "ll/llparser.ml"
         in
        _menhir_goto_block_list_rev _menhir_env _menhir_stack _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (b : (Ll.block)) = _v in
        let _v : (Ll.block) = 
# 227 "ll/llparser.mly"
    ( b )
# 1801 "ll/llparser.ml"
         in
        _menhir_goto_entry_block _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_insn : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.uid * Ll.insn) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | STORE ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | UID _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | BR | RET ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "ll/llparser.mly"
       (string)
# 1830 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (u : (
# 63 "ll/llparser.mly"
       (string)
# 1838 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.operand) = 
# 169 "ll/llparser.mly"
    ( Id u )
# 1843 "ll/llparser.ml"
     in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.operand) = 
# 163 "ll/llparser.mly"
    ( Null )
# 1855 "ll/llparser.ml"
     in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "ll/llparser.mly"
       (int)
# 1862 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 60 "ll/llparser.mly"
       (int)
# 1870 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.operand) = 
# 165 "ll/llparser.mly"
    ( Const (Int64.of_int i) )
# 1875 "ll/llparser.ml"
     in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "ll/llparser.mly"
       (string)
# 1882 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (g : (
# 62 "ll/llparser.mly"
       (string)
# 1890 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.operand) = 
# 167 "ll/llparser.mly"
    ( Gid g )
# 1895 "ll/llparser.ml"
     in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_arg_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ll.ty * Ll.uid) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (a : ((Ll.ty * Ll.uid) list))) = _menhir_stack in
        let _v : ((Ll.ty * Ll.uid) list) = 
# 159 "ll/llparser.mly"
    ( List.rev a )
# 1940 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | ENTRY ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState64 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | CALL ->
                            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | STORE ->
                            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | UID _v ->
                            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                        | BR | RET ->
                            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | STORE ->
                    _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | UID _v ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
                | BR | RET ->
                    _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
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
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_edecl : _menhir_env -> 'ttv_tail -> (Ll.gid * Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Ll.gid * Ll.ty)) = _v in
    let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
    let _v : (Ll.prog) = 
# 91 "ll/llparser.mly"
    ( { ds with edecls = e :: ds.edecls }  )
# 2027 "ll/llparser.ml"
     in
    _menhir_goto_decls_rev _menhir_env _menhir_stack _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 64 "ll/llparser.mly"
       (string)
# 2034 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 64 "ll/llparser.mly"
       (string)
# 2042 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.ginit) = 
# 292 "ll/llparser.mly"
    ( GString s )
# 2047 "ll/llparser.ml"
     in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ginit) = 
# 286 "ll/llparser.mly"
    ( GNull )
# 2059 "ll/llparser.ml"
     in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | COMMA | RBRACKET ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | COMMA | RBRACE ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 60 "ll/llparser.mly"
       (int)
# 2120 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 60 "ll/llparser.mly"
       (int)
# 2128 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.ginit) = 
# 290 "ll/llparser.mly"
    ( GInt (Int64.of_int i) )
# 2133 "ll/llparser.ml"
     in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 62 "ll/llparser.mly"
       (string)
# 2140 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (g : (
# 62 "ll/llparser.mly"
       (string)
# 2148 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.ginit) = 
# 288 "ll/llparser.mly"
    ( GGid g )
# 2153 "ll/llparser.ml"
     in
    _menhir_goto_ginit _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ty_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | RBRACE | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ts : (Ll.ty list))) = _menhir_stack in
        let _v : (Ll.ty list) = 
# 144 "ll/llparser.mly"
    ( List.rev ts )
# 2193 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState19 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (rt : (Ll.ty))), _), _, (ts : (Ll.ty list))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Ll.ty) = 
# 126 "ll/llparser.mly"
    ( Fun (ts, rt) )
# 2212 "ll/llparser.ml"
                 in
                _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState11 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (ts : (Ll.ty list))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ll.ty) = 
# 122 "ll/llparser.mly"
    ( Struct ts )
# 2236 "ll/llparser.ml"
                 in
                _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ll.ty) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
    let _2 = () in
    let _v : (Ll.ty) = 
# 132 "ll/llparser.mly"
    ( Ptr t )
# 2263 "ll/llparser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Ll.ty) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (ts : (Ll.ty list))), _, (t : (Ll.ty))) = _menhir_stack in
            let _2 = () in
            let _v : (Ll.ty list) = 
# 140 "ll/llparser.mly"
    ( t::ts )
# 2312 "ll/llparser.ml"
             in
            _menhir_goto_ty_list_rev _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState11 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | COMMA | RBRACE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty list) = 
# 138 "ll/llparser.mly"
    ( [t] )
# 2334 "ll/llparser.ml"
             in
            _menhir_goto_ty_list_rev _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState26 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (i : (
# 60 "ll/llparser.mly"
       (int)
# 2356 "ll/llparser.ml"
            ))), _, (t : (Ll.ty))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ll.ty) = 
# 124 "ll/llparser.mly"
    ( Array (i,t) )
# 2364 "ll/llparser.ml"
             in
            _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DECLARE | DEFINE | EOF | GID _ | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (tid : (
# 63 "ll/llparser.mly"
       (string)
# 2387 "ll/llparser.ml"
            ))), _, (t : (Ll.ty))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.tid * Ll.ty) = 
# 108 "ll/llparser.mly"
    ( (tid, t) )
# 2394 "ll/llparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (t : (Ll.tid * Ll.ty)) = _v in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _v : (Ll.prog) = 
# 89 "ll/llparser.mly"
    ( { ds with tdecls = t :: ds.tdecls }  )
# 2403 "ll/llparser.ml"
             in
            _menhir_goto_decls_rev _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState37 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | INT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LBRACE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LBRACKET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NULL ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | STRING _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DECLARE | DEFINE | EOF | GID _ | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (g : (
# 62 "ll/llparser.mly"
       (string)
# 2499 "ll/llparser.ml"
            ))), _, (t : (Ll.ty))) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.gid * Ll.ty) = 
# 114 "ll/llparser.mly"
    ( (g, t) )
# 2507 "ll/llparser.ml"
             in
            _menhir_goto_edecl _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
                | COMMA | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState56 in
                    let _v : ((Ll.ty * Ll.uid) list) = 
# 151 "ll/llparser.mly"
    ( [] )
# 2551 "ll/llparser.ml"
                     in
                    _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState60 | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState57 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (u : (
# 63 "ll/llparser.mly"
       (string)
# 2589 "ll/llparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty * Ll.uid) = 
# 147 "ll/llparser.mly"
               ( (t,u) )
# 2595 "ll/llparser.ml"
             in
            (match _menhir_s with
            | MenhirState60 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (a : (Ll.ty * Ll.uid)) = _v in
                let (_menhir_stack, _menhir_s, (args : ((Ll.ty * Ll.uid) list))) = _menhir_stack in
                let _2 = () in
                let _v : ((Ll.ty * Ll.uid) list) = 
# 155 "ll/llparser.mly"
    ( a::args )
# 2607 "ll/llparser.ml"
                 in
                _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
            | MenhirState56 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (a : (Ll.ty * Ll.uid)) = _v in
                let _v : ((Ll.ty * Ll.uid) list) = 
# 153 "ll/llparser.mly"
    ( [a] )
# 2617 "ll/llparser.ml"
                 in
                _menhir_goto_arg_list_rev _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState149 | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BR | CALL | RET | STORE | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 2863 "ll/llparser.ml"
            ))), _, (t1 : (Ll.ty))), _, (o : (Ll.operand))), _, (t2 : (Ll.ty))) = _menhir_stack in
            let _6 = () in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.uid * Ll.insn) = 
# 264 "ll/llparser.mly"
    ( (u, Bitcast (t1,o,t2)) )
# 2871 "ll/llparser.ml"
             in
            _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | BR | CALL | RET | STORE | UID _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (u : (
# 63 "ll/llparser.mly"
       (string)
# 2892 "ll/llparser.ml"
            ))), _, (t : (Ll.ty))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Ll.uid * Ll.insn) = 
# 252 "ll/llparser.mly"
    ( (u, Alloca t) )
# 2899 "ll/llparser.ml"
             in
            _menhir_goto_insn _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | INT _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NULL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | UID _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | LBL _ | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (t : (Ll.ty))) = _menhir_stack in
            let _1 = () in
            let _v : (Ll.terminator) = 
# 203 "ll/llparser.mly"
    ( Ret (t, None) )
# 3014 "ll/llparser.ml"
             in
            _menhir_goto_terminator _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState182 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState184
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState184 in
                    let _v : (Ll.ty list) = 
# 142 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3058 "ll/llparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_ty__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | MenhirState186 | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState185 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | STAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ll.ty))) = _menhir_stack in
            let _v : (Ll.ty list) = 
# 229 "/home/dominik/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 3119 "ll/llparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ty_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonptr_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ll.ty) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (Ll.ty)) = _v in
    let _v : (Ll.ty) = 
# 134 "ll/llparser.mly"
    ( t )
# 3137 "ll/llparser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) = 
# 117 "ll/llparser.mly"
         ( Void )
# 3429 "ll/llparser.ml"
     in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 63 "ll/llparser.mly"
       (string)
# 3436 "ll/llparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (
# 63 "ll/llparser.mly"
       (string)
# 3444 "ll/llparser.ml"
    )) = _v in
    let _v : (Ll.ty) = 
# 128 "ll/llparser.mly"
    ( Namedt t )
# 3449 "ll/llparser.ml"
     in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CROSS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | I1 ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | I64 ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | I8 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LBRACE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LBRACKET ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | UID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | I1 ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | I64 ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | I8 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LBRACE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LBRACKET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | UID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) = 
# 119 "ll/llparser.mly"
       ( I8 )
# 3534 "ll/llparser.ml"
     in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) = 
# 120 "ll/llparser.mly"
        ( I64 )
# 3546 "ll/llparser.ml"
     in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ll.ty) = 
# 118 "ll/llparser.mly"
       ( I1 )
# 3558 "ll/llparser.ml"
     in
    _menhir_goto_nonptr_ty _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_decls_rev : _menhir_env -> 'ttv_tail -> (Ll.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DECLARE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
    | DEFINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | I1 ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | I64 ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | I8 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LBRACE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LBRACKET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | GID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXTERNAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | GLOBAL ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | I1 ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | I64 ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | I8 ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LBRACE ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | LBRACKET ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | UID _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | GLOBAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | UID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TYPE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | I1 ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | I64 ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | I8 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | LBRACE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | LBRACKET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | UID _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
        let _v : (Ll.prog) = 
# 75 "ll/llparser.mly"
    ( { tdecls = List.rev ds.tdecls
      ; gdecls = List.rev ds.gdecls
      ; fdecls = List.rev ds.fdecls
      ; edecls = List.rev ds.edecls
    } )
# 3746 "ll/llparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (ds : (Ll.prog))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 66 "ll/llparser.mly"
       (Ll.prog)
# 3761 "ll/llparser.ml"
            ) = 
# 71 "ll/llparser.mly"
    ( ds )
# 3765 "ll/llparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 66 "ll/llparser.mly"
       (Ll.prog)
# 3772 "ll/llparser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 66 "ll/llparser.mly"
       (Ll.prog)
# 3801 "ll/llparser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ll.prog) = 
# 83 "ll/llparser.mly"
    ( { tdecls = [] ; gdecls = [] ; fdecls = [] ; edecls = [] } )
# 3817 "ll/llparser.ml"
     in
    _menhir_goto_decls_rev _menhir_env _menhir_stack _v)

# 233 "/home/dominik/.opam/system/lib/menhir/standard.mly"
  

# 3824 "ll/llparser.ml"
