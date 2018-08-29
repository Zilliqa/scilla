" Vim syntax file
" Language: Scilla
" Filenames: *.scilla
" Maintainer: Edison LIM (edison@zilliqa.com)
" URL: 

if exists("b:current_syntax")
      finish
endif

setlocal commentstring=(*%s*)

" Scilla is case-sensitive
syn case match

syn keyword scillaKeyword       send match with end fun let in builtin accept import
syn keyword scillaKeyword       Some None _tag Main _sender _amount field _recipient
syn keyword scillaConstant      True False library Emp contract
syn keyword scillaConstant      Nil Zero Succ 
syn keyword scillaConstant      eq add sub mul lt eq concat Cons
syn keyword scillaConstant      substr dist sha256 put get remove contains[] blt badd
syn keyword scillaTypes         Uint32 Int32 Uint64 Int64 Uint128 Int128 Uint256 Int256
syn keyword scillaTypes         String Bool Map BNum Option List Message ByStr20 ByStr32
syn keyword scillaStdlib        ListUtils BoolUtils NatUtils PairUtils
syn keyword scillaStdlibMethods andb orb negb list_head list_tail list_filter list_map list_append list_reverse
syn keyword scillaStdlibMethods list_sort list_exists list_forall list_find list_zip list_zip_with list_unzip list_to_map list_nth 
syn keyword scillaStdlibMethods nat_prev is_some_zero nat_eq nat_to_int uint32_to_nat_helper uint32_to_nat uint64_to_nat uint128_to_nat int32_to_nat int64_to_nat int128_to_nat
syn keyword scillaStdlibMethods fst snd


syn match scillaOperators           /\(!\||\|&\|+\|-\|<\|>\|=\|%\|\/\|*\|\~\|\^\)/
syn match scillaNumber              /\<-\=\d\+L\=\>\|\<0[xX]\x\+\>/

syn region scillaString             start=+"+  skip=+\\\\\|\\$"+  end=+"+
syn region scillaString             start=+'+  skip=+\\\\\|\\$'+  end=+'+
syn region scillaComment            start="(\*" end="\*)" fold

hi def link scillaKeyword       Keyword
hi def link scillaConstant      Constant
hi def link scillaTypes         Type
hi def link scillaNumber		Number
hi def link scillaString        String
hi def link scillaComment       Comment
hi def link scillaOperators		Operator
hi def link scillaStdlib        Keyword
hi def link scillaStdlibMethods Function

" Transition syntax
syn match scillaTransition 	/\<transition\>/ nextgroup=transitionName skipwhite
syn match transitionName 	contained /\<[a-zA-Z_$][0-9a-zA-Z_$]*/ skipwhite

hi def link scillaTransition Type
hi def link transitionName Function


