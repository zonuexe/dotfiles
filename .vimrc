"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/megurine/.vim/dein/repos/github.com/Shougo/dein.vim
set runtimepath+=/home/tadsan/.vim/dein/repos/github.com/Shougo/dein.vim
set runtimepath+=/home/megurinel/.vim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state(expand('~/.vim/dein'))
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
   call dein#add('scrooloose/nerdtree')
   call dein#add('mattn/emmet-vim')
   call dein#add('StanAngeloff/php.vim')

   " You can specify revision/branch/tag.
   call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
   call dein#add('Shougo/vimshell')

   " Required:
   call dein#end()
   call dein#save_state()
endif

" Required:
filetype plugin indent on

nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

nnoremap <silent><C-e> :NERDTreeToggle<CR>

" Coding defaults

set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=1
set backspace=indent,eol,start

" PHP
let php_sql_query = 1
let php_baselib = 1
let php_htmlInStrings = 1
let php_noShortTags = 1
let php_parent_error_close = 1
" g:php_syntax_extensions_enabled


" Terminal titlebar
let &t_ti .= "\e[22;0t"
let &t_te .= "\e[23;0t"

" SQL
let g:sql_type_default='mysql'

syntax on

function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END
