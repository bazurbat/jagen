package {
    source = 'cairo-1.14.2.tar.xz',
    build = {
        type    = 'GNU',
        options = {
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
        libs = { 'cairo' }
    },
    requires = {
        'fontconfig',
        'freetype',
        'pixman',
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
