local Command = require 'Command'
local P = {}

local Log = require 'Log'

function P.quote(...)
    local function quote(arg)
        return string.format('%q', tostring(arg))
    end
    return table.concat(table.imap({...}, quote), ' ')
end

function P.expand(str)
    local command = string.format("printf '%%s' %q", str)
    local file    = assert(io.popen(command))
    local output  = file:read('*a')
    file:close()
    return output
end

function P.mkpath(...)
    local sep = '/'
    local path = {}
    for i = 1, select('#', ...) do
        local arg = select(i, ...)
        if arg == nil then error("bad argument #"..i.." to 'mkpath' (string expected, got nil)") end
        if #arg > 0 then
            table.insert(path, arg)
        end
    end
    return table.concat(path, sep)
end

function P.exec(cmdline, ...)
    Log.debug1(cmdline, ...)
    local command = string.format(cmdline, ...)
    local status = os.execute(command)
    -- handling API change in Lua 5.2
    if type(status) == 'number' then
        return status == 0, status % 0xFF
    else
        return status or false
    end
end

function P.popen(cmdline, ...)
    Log.debug1(cmdline, ...)
    local prog = string.format(cmdline, ...)
    return assert(io.popen(prog))
end

function P.pread(format, cmdline, ...)
    local file = P.popen(cmdline, ...)
    local out = file:read(format)
    file:close()
    return out
end

function P.getenv(vars)
    local o = {}
    for _, v in ipairs(vars) do
        local value = os.getenv(v)
        assert(value and #value > 0,
            string.format("the environment variable '%s' is not set", v))
        table.insert(o, value)
    end
    return o
end

function P.rmrf(...)
    if select('#', ...) > 0 then
        return Command:new('rm -rf', quote(...)):exec()
    end
    return true
end

function P.create_file(path)
    if not io.open(path) then
        assert(io.open(path, 'w')):close()
    end
end

function P.mkdir(...)
    return P.exec('mkdir -p %s', P.quote(...))
end

function P.exists(path)
    return P.exec('test -e "%s"', path)
end

function P.file_exists(path)
    return P.exec('test -f "%s"', path)
end

function P.dir_exists(path)
    return P.exec('test -d "%s"', path)
end

-- Returns true if both arguments are existing directories which have the same
-- physical path.
function P.same_dir(dir1, dir2)
    -- `cd ""` (with empty dir) does nothing which may lead to surprising
    -- result if empty string is supplied as an argument
    return Command:newf([[dir1="%s"; dir2="%s"
dir1="$([ "$dir1" ] && cd "$dir1" 2>&- && pwd -P)"
dir2="$([ "$dir2" ] && cd "$dir2" 2>&- && pwd -P)"
test "$dir1" -a "$dir2" && test "$dir1" = "$dir2"]],
        assert(dir1), assert(dir2)):exec()
end

-- Returns true if dir2 is the subdirectory of dir1
function P.is_subdir(dir1, dir2)
    return Command:newf([[dir1="%s" dir2="%s"
if [ -d "$dir1" ]; then dir1=$(cd "$dir1" 2>&- && pwd -P); fi
if [ -d "$dir2" ]; then dir2=$(cd "$dir2" 2>&- && pwd -P); fi
test "$dir1" -a "$dir2" && test "$dir2" != "${dir2#$dir1}"]],
        assert(dir1), assert(dir2)):exec()
end

-- Returns true if 'dir' is an existing directory and does not have the same
-- physical path as other directory or the other directory does not exist.
function P.can_delete_safely(dir, other_dir)
    return Command:newf([[dir1="%s"; dir2="%s"
dir1="$([ "$dir1" ] && cd "$dir1" 2>&- && pwd -P)"
dir2="$([ "$dir2" ] && cd "$dir2" 2>&- && pwd -P)"
test "$dir1" && test "$dir1" != "$dir2"]],
        assert(dir), other_dir or ''):exec()
end

function P.is_empty(path)
    return Command:new('cd', quote(assert(path)), '2>/dev/null', '&&', 'ls -A'):read() == nil
end

function P.dirname(path)
    return P.pread('*l', 'dirname "%s"', path)
end

return P
