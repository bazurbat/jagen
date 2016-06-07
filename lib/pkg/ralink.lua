return {
    source = 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2',
    patches = {
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot', 1 },
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt',     1 }
    },
    build = {
        type = 'make',
        in_source = true
    },
    install = {
        modules = 'os/linux'
    }
}
