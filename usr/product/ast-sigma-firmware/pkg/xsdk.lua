return {
    source = 'CPU_KEYS_SMP86xx_2010-02-12.tar.gz',
    export = {
        env = {
            XSDK_ROOT = '$pkg_source_dir/signed_items',
            XSDK_DEFAULT_KEY_DOMAIN   = '8644_ES1_prod',
            XSDK_DEFAULT_ZBOOT_CERTID = '0000',
            XSDK_DEFAULT_CPU_CERTID   = '0001'
        }
    }
}
