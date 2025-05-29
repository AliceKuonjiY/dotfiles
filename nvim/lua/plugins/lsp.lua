return {
    "neovim/nvim-lspconfig",
    opts = {
        inlay_hints = {
            enabled = true,
            exclude = { "vue", "c" }, -- filetypes for which you don't want to enable inlay hints
        },
    }
}
