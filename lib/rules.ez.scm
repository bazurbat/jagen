(pkg 'rootfs)

(pkg 'u-boot
     '(config min
              (build (rootfs patch)))
     '(config target
              (build (u-boot build min))
              (mkimage)))

(pkg 'linux
     '(config target
              (build)
              (install (rootfs patch))
              (depmod (cmem install)
                      (syslink install))))

(pkg 'cmem
     '(unpack (rootfs patch))
     '(build (linux install target))
     '(install))

(pkg 'syslink
     '(unpack (rootfs patch))
     '(build (linux install target))
     '(install))

(when (string=? "Debug" (env 'build-type))
  (pkg 'gdb
       '(config host
                (build)
                (install))))

(if #f
  (begin

(pkg 'chibi-scheme
     '(config target
              (unpack)
              (build (chibi-scheme install host))
              (install)))

(pkg 'chicken
     '(config host
              (build)
              (install))
     '(config cross
              (build after (chicken install host))
              (install))
     '(config target
              (build after (chicken install cross))
              (install (rootfs patch))))

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
              (install (rootfs patch))))

(pkg 'luajit
     '(config host
              (unpack)
              (build)
              (install))
     '(config target
              (unpack)
              (build (luajit install host))
              (install (rootfs patch))))

(pkg 'ffmpeg
     '(config host
              (build)
              (install))
     '(config target
              (build)
              (install (rootfs patch))))

(pkg 'soundtouch
     '(config target
              (build)
              (install (rootfs patch))))

))
