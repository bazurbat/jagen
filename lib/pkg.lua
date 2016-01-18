--{{{ Target

Target = {}
Target.__index = Target

function Target:new(name, stage, config)
    local target = {
        name   = name,
        stage  = stage,
        config = config,
        inputs = {}
    }
    setmetatable(target, self)
    return target
end

function Target:from_rule(rule, name, config)
    local stage = rule[1]; assert(type(stage) == 'string')
    local target = Target:new(name, stage, config)

    for i = 2, #rule do
        local input = rule[i]
        table.insert(target.inputs, Target:new(input[1], input[2], input[3]))
    end

    return target
end

function Target:from_arg(arg)
    local name, stage, config
    local c = string.split(arg, ':')

    if c[1] and #c[1] > 0 then
        name = c[1]
    end
    if c[2] and #c[2] > 0 then
        stage = c[2]
    end
    if c[3] and #c[3] > 0 then
        config = c[3]
    end

    return Target:new(name, stage, config)
end

function Target:__eq(other)
    return self.name   == other.name   and
           self.stage  == other.stage  and
           self.config == other.config
end

function Target:__tostring(sep)
    local o = {}
    sep = sep or '-'
    if self.name   then table.insert(o, self.name)   end
    if self.stage  then table.insert(o, self.stage)  end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, sep)
end

function Target:add_inputs(target)
    for _, i in ipairs(target.inputs) do
        local function eq(t)
            return t == i
        end
        local found = find(eq, self.inputs)
        if not found then
            table.insert(self.inputs, i)
        end
    end

    return self
end

--}}}
--{{{ Package

Package = {
    init_stages = { 'unpack', 'patch' }
}
Package.__index = Package

function Package:__tostring()
    local o = {}
    if self.name then table.insert(o, self.name) end
    if self.config then table.insert(o, self.config) end
    return table.concat(o, ':')
end

function Package:read(rule)
    if type(rule[1]) == 'string' then
        rule.name = rule[1]
        table.remove(rule, 1)
    end
    if type(rule[1]) == 'string' then
        rule.config = rule[1]
        table.remove(rule, 1)
    end
    if type(rule.source) == 'string' then
        rule.source = { type = 'dist', location = rule.source }
    end
    return rule
end

function Package:load(filename)
    local pkg = {}
    local env = {}
    function env.package(rule)
        pkg = Package:read(rule)
    end
    local chunk = loadfile(filename)
    if chunk then
        setfenv(chunk, env)
        chunk()
    end
    return pkg
end

function Package:create(name)
    local pkg = { name = name, stages = {} }
    setmetatable(pkg, self)
    self.__index = self

    for _, s in ipairs(self.init_stages) do
        pkg:add_target(Target:new(name, s))
    end

    for filename in each(jagen.import_paths('pkg/'..name..'.lua')) do
        table.merge(pkg, pkg:load(filename))
    end

    return pkg
end

function Package:add_target(target)
    local function default(this)
        for _, stage in ipairs(self.init_stages) do
            if stage == target.stage and stage == this.stage then
                return this
            end
        end
    end
    local function eq(this)
        return this == target or default(this)
    end

    local found = find(eq, self.stages)
    if found then
        jagen.debug2(tostring(self), '=', tostring(target))
        found:add_inputs(target)
        return self
    else
        jagen.debug2(tostring(self), '+', tostring(target))
        table.insert(self.stages, target)
    end

    return self
end

function Package:add_build_targets(config)
    local source = self.source
    local build = self.build
    if source then
        if source.type == 'repo' then
            jagen.need_repo = true
            self:add_target(Target:from_rule({ 'unpack',
                        { 'repo', 'unpack' }
                }, self.name))
        end
    end
    if build then
        if build.type == 'GNU' then
            if build.generate or build.autoreconf then
                jagen.need_libtool = true
                self:add_target(Target:from_rule({ 'autoreconf',
                            { 'libtool', 'install', 'host' }
                    }, self.name))
            end
        end
        if build.type then
            if config == 'target' then
                self:add_target(Target:from_rule({ 'build',
                            { 'toolchain', 'install', 'target' }
                    }, self.name, config))
            else
                self:add_target(Target:from_rule({ 'build',
                    }, self.name, config))
            end
            self:add_target(Target:from_rule({ 'install'
                }, self.name, config))
        end
    end
end

function Package:add_ordering_dependencies()
    local prev, common

    for _, s in ipairs(self.stages) do
        if prev then
            if common and s.config ~= prev.config then
                table.insert(s.inputs, 1, common)
            else
                table.insert(s.inputs, 1, prev)
            end
        end

        prev = s
        if not s.config then
            common = s
        end
    end
end

--}}}
