"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/megurine/.vim/dein/repos/github.com/Shougo/dein.vim
set runtimepath+=/home/tadsan/.vim/repos/github.com/Shougo/dein.vim

" Required:
call dein#begin(expand('~/.vim/dein'))

" Let dein manage dein
" Required:
call dein#add('Shougo/dein.vim')

" Add or remove your plugins here:
call dein#add('Shougo/neosnippet.vim')
call dein#add('Shougo/neosnippet-snippets')
call dein#add('itchyny/calendar.vim')
call dein#add('editorconfig/editorconfig-vim')

" You can specify revision/branch/tag.
call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
call dein#add('Shougo/vimshell')

" Required:
call dein#end()

" Required:
filetype plugin indent on


" Use vsplit mode
" http://qiita.com/kefir_/items/c725731d33de4d8fb096
if has("vim_starting") && !has('gui_running') && has('vertsplit')
  function! g:EnableVsplitMode()
    " enable origin mode and left/right margins
    let &t_CS = "y"
    let &t_ti = &t_ti . "\e[?6;69h"
    let &t_te = "\e[?6;69l" . &t_te
    let &t_CV = "\e[%i%p1%d;%p2%ds"
    call writefile([ "\e[?6h\e[?69h" ], "/dev/tty", "a")
  endfunction

  " old vim does not ignore CPR
  map <special> <Esc>[3;9R <Nop>

  " new vim can't handle CPR with direct mapping
  " map <expr> ^[[3;3R g:EnableVsplitMode()
  set t_F9=^[[3;3R
  map <expr> <t_F9> g:EnableVsplitMode()
  let &t_RV .= "\e[?6;69h\e[1;3s\e[3;9H\e[6n\e[0;0s\e[?6;69l"
endif

nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

nnoremap <silent><C-e> :NERDTreeToggle<CR>

set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=1

" PHP
let php_sql_query = 1
let php_baselib = 1
let php_htmlInStrings = 1
let php_noShortTags = 1
let php_parent_error_close = 1

" SQL
let g:sql_type_default='mysql'

syntax on
