return {
    "mfussenegger/nvim-jdtls",
    dependencies = { "folke/which-key.nvim" },
    ft = java_filetypes,
    opts = function()
        return {
            root_dir = require("lspconfig.server_configurations.jdtls").default_config.root_dir,

            -- How to find the project name for a given root dir.
            project_name = function(root_dir)
                return root_dir and vim.fs.basename(root_dir)
            end,

            -- Where are the config and workspace dirs for a project?
            jdtls_config_dir = function(project_name)
                return vim.fn.stdpath("cache") .. "/jdtls/" .. project_name .. "/config"
            end,
            jdtls_workspace_dir = function(project_name)
                return vim.fn.stdpath("cache") .. "/jdtls/" .. project_name .. "/workspace"
            end,

            cmd = {
                "jdtls",
                "--jvm-arg=" .. string.format(
                    "-javaagent:%s",
                    vim.fn.expand("$HOME/.local/share/nvim/mason/share/jdtls/lombok.jar")
                ),
            },
            full_cmd = function(opts)
                local fname = vim.api.nvim_buf_get_name(0)
                local root_dir = opts.root_dir(fname)
                local project_name = opts.project_name(root_dir)
                local cmd = vim.deepcopy(opts.cmd)
                if project_name then
                    vim.list_extend(cmd, {
                        "-configuration",
                        opts.jdtls_config_dir(project_name),
                        "-data",
                        opts.jdtls_workspace_dir(project_name),
                    })
                end
                return cmd
            end,
            dap = { hotcodereplace = "auto", config_overrides = {} },
            dap_main = {},
            test = true,
            settings = {
                java = {
                    inlayHints = {
                        parameterNames = {
                            enabled = "all",
                        },
                    },
                },
            },
        }
    end,
    config = function(_, opts)
        local mason_registry = require("mason-registry")
        local bundles = {}
        if opts.dap and LazyVim.has("nvim-dap") and mason_registry.is_installed("java-debug-adapter") then
            local java_dbg_pkg = mason_registry.get_package("java-debug-adapter")
            local java_dbg_path = java_dbg_pkg:get_install_path()
            local jar_patterns = {
                java_dbg_path .. "/extension/server/com.microsoft.java.debug.plugin-*.jar",
            }
            -- java-test also depends on java-debug-adapter.
            if opts.test and mason_registry.is_installed("java-test") then
                local java_test_pkg = mason_registry.get_package("java-test")
                local java_test_path = java_test_pkg:get_install_path()
                vim.list_extend(jar_patterns, {
                    java_test_path .. "/extension/server/*.jar",
                })
            end
            for _, jar_pattern in ipairs(jar_patterns) do
                for _, bundle in ipairs(vim.split(vim.fn.glob(jar_pattern), "\n")) do
                    table.insert(bundles, bundle)
                end
            end
        end
        vim.api.nvim_create_autocmd("LspAttach", {
            callback = function(args)
                local client = vim.lsp.get_client_by_id(args.data.client_id)
                if client and client.name == "jdtls" then
                    local wk = require("which-key")
                    wk.register({
                        ["<leader>cx"] = { name = "+extract" },
                        ["<leader>cxv"] = { require("jdtls").extract_variable_all, "Extract Variable" },
                        ["<leader>cxc"] = { require("jdtls").extract_constant, "Extract Constant" },
                        ["gs"] = { require("jdtls").super_implementation, "Goto Super" },
                        ["gS"] = { require("jdtls.tests").goto_subjects, "Goto Subjects" },
                        ["<leader>co"] = { require("jdtls").organize_imports, "Organize Imports" },
                    }, { mode = "n", buffer = args.buf })
                    wk.register({
                        ["<leader>c"] = { name = "+code" },
                        ["<leader>cx"] = { name = "+extract" },
                        ["<leader>cxm"] = {
                            [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]],
                            "Extract Method",
                        },
                        ["<leader>cxv"] = {
                            [[<ESC><CMD>lua require('jdtls').extract_variable_all(true)<CR>]],
                            "Extract Variable",
                        },
                        ["<leader>cxc"] = {
                            [[<ESC><CMD>lua require('jdtls').extract_constant(true)<CR>]],
                            "Extract Constant",
                        },
                    }, { mode = "v", buffer = args.buf })

                    if opts.dap and LazyVim.has("nvim-dap") and mason_registry.is_installed("java-debug-adapter") then
                        -- custom init for Java debugger
                        require("jdtls").setup_dap(opts.dap)
                        require("jdtls.dap").setup_dap_main_class_configs(opts.dap_main)

                        -- Java Test require Java debugger to work
                        if opts.test and mason_registry.is_installed("java-test") then
                            -- custom keymaps for Java test runner (not yet compatible with neotest)
                            wk.register({
                                ["<leader>t"] = { name = "+test" },
                                ["<leader>tt"] = { require("jdtls.dap").test_class, "Run All Test" },
                                ["<leader>tr"] = { require("jdtls.dap").test_nearest_method, "Run Nearest Test" },
                                ["<leader>tT"] = { require("jdtls.dap").pick_test, "Run Test" },
                            }, { mode = "n", buffer = args.buf })
                        end
                    end

                    -- User can set additional keymaps in opts.on_attach
                    if opts.on_attach then
                        opts.on_attach(args)
                    end
                end
            end,
        })
    end,
}
