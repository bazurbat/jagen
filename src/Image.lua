local system = require 'system'

local Image = {}
local padding = 8

local function calculate_size(dir)
    return tonumber(
        system.pipe(function (f) return string.match(f:read(), '%d+') end,
        string.format('du -sm "%s"', dir)))
end

local function run(format, ...)
    if not system.fexec(format, ...) then
        local command = string.format(format, ...)
        error('failed to run: '..command, 2)
    end
end

local function srun(format, ...)
    return run('sudo '..format, ...)
end

function Image:create(src_dir, out_file)
    local size = calculate_size(src_dir) + padding
    local mount_path = '/mnt/image'
    local ok, err

    run('dd if=/dev/zero of="%s" bs=1 count=0 seek=%sM', out_file, size)
    run('mke2fs -F -t ext4 -O ^huge_file "%s" %sM', out_file, size)

    srun('mkdir -p "%s"', mount_path)
    srun('mount -t ext4 "%s" "%s"', out_file, mount_path)
    ok, err = pcall(srun, 'rsync -a "%s/" "%s"', src_dir, mount_path)
    srun('umount "%s"', mount_path)
    if not ok then
        error(err)
    end
end

return Image
