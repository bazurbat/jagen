return {
    source = {
        type      = 'dist',
        location  = 'https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.14.33.tar.xz',
        sha256sum = 'c380e2b4318d9ef0517c29145cf6fa1f6f437f38e93827ccd772184f77b90831'
    },
    build = {
        type   = 'linux-kernel',
        arch   = 'x86',
        config = 'x86_64_defconfig',
        image  = 'bzImage',
    }
}
