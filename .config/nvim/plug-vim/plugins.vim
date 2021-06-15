call plug#begin('~/.config/nvim/autoload/plugged')

    "Plugins
    
    "Auto Pairs
    Plug 'jiangmiao/auto-pairs'

    "File Explorer
    "NERDTree
    Plug 'scrooloose/NERDTree'
    Plug 'preservim/nerdTree'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    " Plug 'airblade/vim-rooter'

    "ranger
    Plug 'kevinhwang91/rnvimr', {'do': 'make sync'}

    "Theme
    Plug 'joshdick/onedark.vim'
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'morhetz/gruvbox'
    Plug 'arcticicestudio/nord-vim'
    Plug 'sonph/onehalf', { 'rtp': 'vim' }
    Plug 'crusoexia/vim-monokai'

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

    "EasyMotion
    Plug 'easymotion/vim-easymotion'

    "Colors
    Plug 'norcalli/nvim-colorizer.lua'

    "Laravel
    Plug 'tpope/vim-dispatch'             
    Plug 'tpope/vim-projectionist'        
    Plug 'noahfrederick/vim-composer'    
    Plug 'noahfrederick/vim-laravel'

    "Telescope
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'   
    Plug 'nvim-telescope/telescope-fzy-native.nvim'

call plug#end()


