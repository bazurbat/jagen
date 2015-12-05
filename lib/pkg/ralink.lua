package {
    name   = 'ralink',
    source = 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022.tar.bz2',
    patches = {
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-no-tftpboot', 1 },
        { 'DPO_RT5572_LinuxSTA_2.6.1.3_20121022-encrypt',     1 }
    },
    build = {
        type = 'GNU',
        in_source = true
    }
}
