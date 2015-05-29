
Ninja = {}

function Ninja:format_inputs(inputs)
    local sep = string.format(' $\n%s', Jagen.format_indent(16))
    local t = {}
    for _, d in ipairs(inputs) do
        table.insert(t, tostring(d))
    end
    return table.concat(t, sep)
end

function Ninja:generate(packages, out_file, in_file)
    local out = io.open(out_file, 'w')

    out:write(string.format('builddir = %s\n\n', env('pkg_build_dir')))
    out:write(string.format('rule command\n'))
    out:write(string.format('    command = $command\n\n'))
    out:write(string.format('rule script\n'))
    out:write(string.format('    command = ' .. env('pkg_bin_dir') .. '/$script && touch $out\n\n'))

    local sep = string.format(' $\n%s', Jagen.format_indent(16))

    for i, pkg in ipairs(packages) do
        local pn = pkg.name
        for j, stage in ipairs(pkg.stages or {}) do
            local sn = stage.stage
            local sc = stage.config
            out:write(string.format('build %s: script', tostring(stage)))
            if #stage.inputs > 0 then
                out:write(' $\n' .. Jagen.format_indent(16))
                out:write(Ninja:format_inputs(stage.inputs))
            end
            out:write('\n')
            out:write(string.format('    script = jagen-pkg %s %s', pn, sn))
            if sc then
                out:write(' ', sc)
            end
            out:write('\n')
        end
        out:write("\n")
    end

    out:close()
end
