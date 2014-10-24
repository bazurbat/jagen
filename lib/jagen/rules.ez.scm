(pkg 'rootfs
     '(unpack)
     '(prepare))

(pkg 'u-boot
     '(config min
              (build (rootfs prepare)))
     '(config target
              (build (u-boot build min))
              (scr)))

(pkg 'linux
     '(config target
              (build)
              (install (rootfs prepare))
              (depmod (linux-cmem install)
                      (syslink install))))

(pkg 'linux-cmem
     '(unpack (rootfs prepare))
     '(build (linux install))
     '(install))

(pkg 'syslink
     '(unpack (rootfs prepare))
     '(build (linux install))
     '(install))

(pkg 'chibi-scheme
     '(config target
              (unpack)
              (build (chibi-scheme install host))
              (install)))

(pkg 'chicken
     '(unpack)
     '(prepare)
     '(config host
              (build)
              (install))
     '(config cross
              (build after (chicken install host))
              (install))
     '(config target
              (build after (chicken install cross))
              (install (rootfs prepare))))

(pkg 'libuv
     '(config host
              (unpack)
              (prepare)
              (build)
              (install))
     '(config target
              (unpack)
              (prepare)
              (build (libuv install host))
              (install (rootfs prepare))))

(pkg 'luajit
     '(config host
              (unpack)
              (build)
              (install))
     '(config target
              (unpack)
              (build (luajit install host))
              (install (rootfs prepare))))

(pkg 'ffmpeg
     '(unpack)
     '(config host
              (build)
              (install))
     '(config target
              (build)
              (install (rootfs prepare))))

(pkg 'soundtouch
     '(unpack)
     '(prepare)
     '(config target
              (build)
              (install (rootfs prepare))))
