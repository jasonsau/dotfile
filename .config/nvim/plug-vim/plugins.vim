call plug#begin('~/.local/share/nvim/site/autoload/plugged')

    "Plugins
    
    "Auto Pairs
    Plug 'jiangmiao/auto-pairs'

    "File Explorer
    "NERDTree
    Plug 'scrooloose/NERDTree'
    Plug 'preservim/nerdTree'
     Plug 'airblade/vim-rooter'

    "Theme
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'morhetz/gruvbox'
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
    Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

    " Indent guides
    Plug 'Yggdroot/indentLine' 

    "Close tag
    Plug 'alvan/vim-closetag'

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

    "Plugis para prueba
    Plug 'neovim/nvim-lspconfig'
    Plug 'glepnir/lspsaga.nvim'
    Plug 'karb94/neoscroll.nvim'
    Plug 'tpope/vim-surround'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'natebosch/vim-lsc'
    Plug 'mfussenegger/nvim-jdtls'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'L3MON4D3/LuaSnip' 
    Plug 'hrsh7th/cmp-buffer'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'sbdchd/neoformat'
    Plug 'APZelos/blamer.nvim'
    call plug#end()



Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'


Plug 'preservim/nerdtree' |
            \ Plug 'Xuyuanp/nerdtree-git-plugin'
