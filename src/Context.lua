
local Context = {}

function Context:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Context:__rawtostring()
    local saved = Context.__tostring
    Context.__tostring = nil
    local s = tostring(self)
    Context.__tostring = saved
    return s
end

function Context:__tostring()
    local insert, concat = table.insert, table.concat
    local lines = {}
    local function append(...)
        for i = 1, select('#', ...) do
            insert(lines, (select(i, ...)))
        end
    end
    if self.name then
        append(self.name)
    end
    if self.config then
        append(':', self.config)
    end
    if self.filename then
        if #lines > 0 then append(' ') end
        if self.name or self.config then append('(') end
        local filename, removed = self.filename:remove_prefix(System.dirname(Jagen.root_dir))
        if removed then append('...') end append(filename)
        if self.line then append(':', self.line) end
        if self.name or self.config then append(')') end
    end
    if self.template then
        append(' [', concat(self.template, ', '), ']')
    end
    if self.implicit and #lines > 0 then
        append(' *')
    end
    return table.concat(lines)
end

function Context:__unm()
    local this = self
    local s = ''
    while this do
        for k, v in pairs(this) do
            if k ~= 'parent' then
                s = string.format('%s:%s=%s', s, k, tostring(v))
            end
        end
        this = this.parent
    end
    return s
end

function Context:tokey(with_parent)
    local o = {}
    if with_parent then
        if self.name then
            table.insert(o, self.name)
        end
        if self.filename then
            table.insert(o, self.filename)
        end
        if self.line then
            table.insert(o, self.line)
        end
    end
    if self.config then
        table.insert(o, self.config)
    end
    if self.template then
        table.iextend(o, self.template)
    end
    return table.concat(o, ':')
end

local current_context
local context_stack = {}


local function push_context(new)
    new = Context:new(new)
    new.parent = current_context
    table.insert(context_stack, new)
    current_context = new
    return new
end

local function pop_context()
    local o = assert(table.remove(context_stack))
    current_context = context_stack[#context_stack]
    return o
end


return Context
