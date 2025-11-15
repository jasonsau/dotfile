return {
  {
    {
      "ellisonleao/gruvbox.nvim",
      priority = 1000,
      config = true,
      opts = {
        transparent_mode = true,
        bold = true,
        terminal_colors = true,
        italic = {
          strings = true,
          emphasis = true,
          operators = true,
          folds = true,
        },
      },
    },

    {
      "LazyVim/LazyVim",
      opts = {
        colorscheme = "gruvbox",
      },
    },
  },
}
