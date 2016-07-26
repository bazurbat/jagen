local System = require 'System'
local Log = require 'Log'

local Image = {}

local ast_firmware = '$jagen_host_dir/sbin/ast-firmware'

local function calculate_size(dir)
    return System.pread('*n', 'du -sm "%s"', dir)
end

local function run(format, ...)
    if not System.exec(format, ...) then
        local command = string.format(format, ...)
        error('command failed: '..command, 2)
    end
end

local function srun(format, ...)
    return run('sudo '..format, ...)
end

function Image:create(src_dir, out_file)
    local size = calculate_size(src_dir)
    -- add some padding for filesystem structures
    size = size + math.ceil(size / 12)
    assert(size > 0)

    run('dd if=/dev/zero of="%s" bs=1M count=0 seek=%s 2>/dev/null', out_file, size)
    run('mke2fs -qF -t ext4 -O ^huge_file "%s" %sM', out_file, size)

    local mount_path = System.pread('*l', 'mktemp -d')
    local ok, err

    assert(System.dir_exists(mount_path))

    srun('mount -t ext4 "%s" "%s"', out_file, mount_path)
    ok, err = pcall(srun, 'rsync -a "%s/" "%s"', src_dir, mount_path)
    srun('umount "%s"', mount_path)
    run('rmdir "%s"', mount_path)
    if not ok then
        error(err)
    end

    Log.message('Image created: %s', out_file)
end

function Image:create_firmware(src_dir, out_file, version)
    srun('"%s" create -p "%s" -f "%s" -k "${jagen_private_dir:?}/keys/keyfile.gpg"',
        ast_firmware, src_dir, out_file)
end

return Image
