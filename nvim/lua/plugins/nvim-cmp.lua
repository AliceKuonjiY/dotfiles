return {
    -- Setup nvim-cmp
    {
        "hrsh7th/nvim-cmp",
        version = false, -- last release is way too old
        event = "InsertEnter",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
        },
        opts = function()
            local cmp = require("cmp")
            local defaults = require("cmp.config.default")()
            local auto_select = true
            return {
                mapping = cmp.mapping.preset.insert({
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
                    ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<Tab>"] = LazyVim.cmp.confirm({ select = auto_select }),
                    ["<C-y>"] = LazyVim.cmp.confirm({ select = true }),
                    ["<S-CR>"] = LazyVim.cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    ["<CR>"] = function(fallback)
                        cmp.abort()
                        fallback()
                    end,
                    ["<C-CR>"] = function(fallback)
                        return LazyVim.cmp.map({ "snippet_forward", "ai_accept" }, fallback)()
                    end,
                }),
                sources = cmp.config.sources({
                    { name = "lazydev" },
                    { name = "nvim_lsp" },
                    { name = "path" },
                    { name = "spell" },
                    { name = "calc" },
                    { name = "treesitter" },
                    { name = "buffer" },
                }),
                experimental = {
                    -- only show ghost text when we show ai completions
                    ghost_text = vim.g.ai_cmp and {
                        hl_group = "CmpGhostText",
                    } or false,
                },
                sorting = defaults.sorting,
            }
        end,
    },
}
