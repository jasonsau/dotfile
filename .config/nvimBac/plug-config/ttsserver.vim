lua << EOF
local tss = require'lspconfig'.tsserver
tss.setup{
    cmd = { "typescript-language-server", "--stdio" },
    filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
    init_options = { hostInfo = "neovim" },
    root_dir = require'lspconfig'.util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git", ".js"),
}
EOF
