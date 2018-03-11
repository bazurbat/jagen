return {
    source = {
        type     = 'dist',
        location = 'http://www.netfilter.org/projects/iptables/files/iptables-1.4.21.tar.bz2',
        md5sum   = '536d048c8e8eeebcd9757d0863ebb0c0'
    },
    build  = {
        type    = 'gnu',
        options = {
            '--disable-ipv6',
            '--enable-devel',
            '--disable-libipq'
        },
    },
    install = {
        libs = { 'iptc', 'ip4tc', 'ip6tc', 'xtables' }
    }
}
