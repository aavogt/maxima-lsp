# maxima-lsp

maxima-lsp supports

 - `textDocument/rename` single file scope-aware, error insensitive
 - `textDocument/hover` [./maxima_singlepage.html](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html) lookup
 - `textDocument/completion` local and built-in names with documentation

## installation


```bash
# libpcre3-dev is discontinued
wget https://sourceforge.net/projects/pcre/files/latest/download
unzip download
cd pcre-8.45/
./configure --prefix=/usr --enable-utf --enable-static=false
sudo checkinstall \
  --pkgname=libpcre3-mydev \
  --pkgversion=8.45 \
  --backup=no --nodoc \
  make install

sudo apt install python3-html2text # TODO missing many here

git clone https://github.com/aavogt/maxima-lsp
cd maxima-lsp
make # downloads maxima documentation and installs ~/.cabal/bin/maxima-lsp
```


### nvim lspconfig

```lua
vim.filetype.add({
  extension = {
    mac = 'maxima',
    max = 'maxima',
    mc = 'maxima',
  },
})
local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")
local util = require("lspconfig.util")
if not configs.maxima_lsp then
  configs.maxima_lsp = {
    default_config = {
      cmd = { "maxima-lsp" },
      filetypes = { "maxima" },
      root_dir = util.root_pattern(".git")(vim.uv.cwd())
                    or vim.uv.cwd(),
    },
  }
end
vim.api.nvim_create_autocmd("FileType", {
  pattern = "maxima",
  callback = function()
    vim.lsp.set_log_level("debug")
  end
})
lspconfig.maxima_lsp.setup({})
```
