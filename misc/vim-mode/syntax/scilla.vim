" Vim syntax file
" Language: Scilla
" Filenames: *.scilla
" Maintainer: Edison LIM (edison.limjh@gmail.com)
" URL: https://github.com/edisonljh/vim-scilla 

if exists("b:current_syntax")
      finish
endif

setlocal commentstring=(*%s*)

" Scilla is case-sensitive
syn case match

syn keyword scillaKeyword       send match with end fun let in builtin accept import
syn keyword scillaKeyword       Some None _tag Main _sender _amount field _recipient
syn keyword scillaConstant      True False library Emp contract
syn keyword scillaConstant      ListUtils BoolUtils Nil Zero Succ 
syn keyword scillaConstant      eq add sub mul lt eq concat Cons
syn keyword scillaConstant      substr dist sha256 put get remove contains[] blt badd
syn keyword ScillaTypes         Uint32 Int32 Uint64 Int64 Uint128 Int128 Uint256 Int256
syn keyword ScillaTypes         String Hash Bool Map Address BNum Option List Message

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

" Transition syntax
syn match scillaTransition 	/\<transition\>/ nextgroup=transitionName skipwhite
syn match transitionName 	contained /\<[a-zA-Z_$][0-9a-zA-Z_$]*/ skipwhite

hi def link scillaTransition Type
hi def link transitionName Function


