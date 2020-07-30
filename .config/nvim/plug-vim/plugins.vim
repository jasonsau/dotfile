call plug#begin('~/.config/nvim/autoload/plugged')

    "Plugins
    
    "Auto Pairs
    Plug 'jiangmiao/auto-pairs'

    "File Explorer
    "NERDTree
    Plug 'scrooloose/NERDTree'
    "ranger
    Plug 'kevinhwang91/rnvimr', {'do': 'make sync'}

    " Better Syntax Support
    Plug 'sheerun/vim-polyglot'

    "Theme
    Plug 'joshdick/onedark.vim'

    " Stable version of coc
    Plug 'neoclide/coc.nvim', {'branch': 'release'}

    "airline
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    "Integrations git
    Plug 'mhinz/vim-signify'
    Plug 'tpope/vim-fugitive'   

    "commentary
    Plug 'tpope/vim-commentary'

    "Icons
    Plug 'ryanoasis/vim-devicons'

    " Indent guides
    Plug 'Yggdroot/indentLine' 

    "Close tag
    Plug 'alvan/vim-closetag'
call plug#end()
