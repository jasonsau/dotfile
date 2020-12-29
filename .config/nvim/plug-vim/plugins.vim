call plug#begin('~/.config/nvim/autoload/plugged')

    "Plugins
    
    "Auto Pairs
    Plug 'jiangmiao/auto-pairs'

    "File Explorer
    "NERDTree
    Plug 'scrooloose/NERDTree'
    Plug 'preservim/nerdTree'

    "ranger
    Plug 'kevinhwang91/rnvimr', {'do': 'make sync'}

    " Better Syntax Support
    Plug 'sheerun/vim-polyglot'

    "Theme
    Plug 'joshdick/onedark.vim'
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'morhetz/gruvbox'
    Plug 'arcticicestudio/nord-vim'

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
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

    " Indent guides
    Plug 'Yggdroot/indentLine' 

    "Close tag
    Plug 'alvan/vim-closetag'

    "Brackets Color

    "Easy
    Plug 'easymotion/vim-easymotion'

    "Colors
    Plug 'norcalli/nvim-colorizer.lua' 
    call plug#end()
