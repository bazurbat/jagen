local P = {}

local debug_level = os.getenv('jagen_debug')

function P.message(...)
    io.write('(I) ', string.format(...), '\n')
    io.flush()
end

function P.warning(...)
    io.stderr:write('(W) ', string.format(...), '\n')
    io.stderr:flush()
end

function P.error(...)
    io.stderr:write('(E) ', string.format(...), '\n')
    io.stderr:flush()
end

function P.debug0(...)
    if debug_level then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function P.debug1(...)
    if debug_level >= '1' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

function P.debug2(...)
    if debug_level >= '2' then
        io.write('(D) ', string.format(...), '\n')
        io.flush()
    end
end

return P
