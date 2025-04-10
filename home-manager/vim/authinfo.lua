-- Function to read Anthropic API key from authinfo
local function read_anthropic_api_key()
  local home = os.getenv("HOME")
  local default_authinfo_path = home .. "/.authinfo"
  local authinfo_path = vim.g.authinfo_path or default_authinfo_path

  -- Check if authinfo exists
  local file_path = nil

  local f = io.open(authinfo_path, "r")
  if f then
    file_path = authinfo_path
    f:close()
  end

  if not file_path then
    vim.notify("Could not find authinfo file", vim.log.levels.WARN)
    return nil
  end

  -- Read the authinfo file
  local content = ""
  -- Read plain text file
  local f = io.open(file_path, "r")
  if f then
    content = f:read("*a")
    f:close()
  end

  -- Parse the content to find the Anthropic API key
  for line in content:gmatch("[^\r\n]+") do
    -- Look for a line with "machine api.anthropic.com" and extract the password
    if line:match("machine%s+api%.anthropic%.com") then
      local api_key = line:match("password%s+([^%s]+)")
      if api_key then
        return api_key
      end
    end
  end

  vim.notify("Could not find Anthropic API key in authinfo", vim.log.levels.WARN)
  return nil
end
-- Set the environment variable if the key is found, but do it after initialization
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    local api_key = read_anthropic_api_key()
    if api_key then
      vim.env.ANTHROPIC_API_KEY = api_key
      vim.notify("ANTHROPIC_API_KEY has been set from authinfo", vim.log.levels.INFO)
    end
  end,
  once = true,
})
