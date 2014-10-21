(pkg rootfs
     (unpack)
     (prepare (rootfs unpack)))

(pkg u-boot
     (config min
             (pkg (rootfs prepare)))
     (pkg (u-boot build min))
     (scr (u-boot build)))

(pkg linux
     (pkg)
     (install (linux build)
              (rootfs prepare))
     (depmod (linux install)
             (linux-cmem install)
             (syslink install)))

(pkg linux-cmem
     (unpack (rootfs prepare))
     (pkg (linux-cmem unpack)
          (linux install))
     (install (linux-cmem build)))

(pkg syslink
     (unpack (rootfs prepare))
     (pkg (syslink unpack)
          (linux install))
     (install (syslink build)))

(pkg chibi-scheme
     (config host
             (unpack)
             (pkg (chibi-scheme unpack host))
             (install (chibi-scheme build host)))
     (unpack)
     (pkg (chibi-scheme unpack)
          (chibi-scheme install host))
     (install (chibi-scheme build)))

(pkg chicken
     (unpack)
     (prepare (chicken unpack))
     (pkg (chicken prepare)
          (after (chicken install cross)))
     (install (chicken build)
              (rootfs prepare))
     (config host
             (pkg (chicken prepare))
             (install (chicken build host)))
     (config cross
             (pkg (chicken prepare)
                  (after (chicken install host)))
             (install (chicken build cross))))

(pkg libuv
     (config host
             (unpack)
             (prepare (libuv unpack host))
             (pkg (libuv prepare host))
             (install (libuv build host)))
     (unpack)
     (prepare (libuv unpack))
     (pkg (libuv prepare) (libuv install host))
     (install (libuv build) (rootfs prepare)))

(pkg luajit
     (config host
             (unpack)
             (pkg (luajit unpack host))
             (install (luajit build host)))
     (unpack)
     (pkg (luajit unpack) (luajit install host))
     (install (luajit build) (rootfs prepare)))

(pkg ffmpeg
     (unpack)
     (pkg (ffmpeg unpack))
     (install (ffmpeg build) (rootfs prepare))
     (config host
             (pkg (ffmpeg unpack))
             (install (ffmpeg build host))))

(pkg soundtouch
     (unpack)
     (prepare (soundtouch unpack))
     (pkg (soundtouch prepare))
     (install (soundtouch build) (rootfs prepare)))
