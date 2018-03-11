return {
    source = {
        type      = 'dist',
        location  = 'https://cairographics.org/releases/cairo-1.14.2.tar.xz',
        sha256sum = 'c919d999ddb1bbbecd4bbe65299ca2abd2079c7e13d224577895afa7005ecceb'
    },
    build = {
        type = 'gnu',
        -- fails to find lz otherwise and other build failures assuming zlib in
        -- default paths
        configure_needs_install_dir = true,
        options = {
            '--disable-silent-rules',
            '--disable-static',
            '--disable-gtk-doc',
            '--disable-gtk-doc-html',
            '--disable-valgrind',
            '--disable-xlib',
            '--disable-xlib-xrender',
            '--disable-xcb',
            '--disable-xlib-xcb',
            '--disable-xcb-shm',
            '--disable-qt',
            '--disable-quartz',
            '--disable-quartz-font',
            '--disable-quartz-image',
            '--disable-win32',
            '--disable-win32-font',
            '--disable-skia',
            '--disable-os2',
            '--disable-beos',
            '--disable-drm',
            '--disable-gallium',
            '--enable-png',
            '--disable-gl',
            '--disable-glesv2',
            '--disable-cogl',
            '--disable-directfb',
            '--disable-vg',
            '--disable-egl',
            '--disable-glx',
            '--disable-wgl',
            '--disable-script',
            '--enable-ft',
            '--enable-fc',
            '--disable-ps',
            '--disable-pdf',
            '--disable-svg',
            '--disable-test-surfaces',
            '--disable-tee',
            '--disable-xml',
            '--enable-pthread=auto',
            '--disable-gobject',
            '--disable-trace',
            '--disable-interpreter',
            '--disable-symbol-lookup',
            '--without-x',
            '--without-skia',
            '--without-gallium',
        },
    },
    install = {
        libs = { 'cairo' }
    },
    requires = {
        'fontconfig',
        'freetype',
        'libpng',
        'pixman',
        'zlib',
    }
}
--[[ NOTES:
Fontconfig is needed to properly compile cairo:
  ../pango/.libs/libpangocairo-1.0.so: undefined reference to `cairo_ft_font_options_substitute'
  ../pango/.libs/libpangocairo-1.0.so: undefined reference to `cairo_ft_font_face_create_for_pattern'
PNG is needed for cairo too:
  test-pangocairo-threads.c:(.text.startup+0x210): undefined reference to `cairo_surface_write_to_png'
  test-pangocairo-threads.c:(.text.startup+0x220): undefined reference to `cairo_surface_write_to_png'
]]
