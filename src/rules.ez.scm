(pkg rootfs
     (unpack)
     (prepare (rootfs unpack)))

(pkg u-boot
     (config min
             (build (rootfs prepare)))
     (build (u-boot build min))
     (scr (u-boot build)))

(pkg linux
     (build)
     (install (linux build)
              (rootfs prepare))
     (depmod (linux install)
             (linux-cmem install)
             (syslink install)))

(pkg linux-cmem
     (unpack (rootfs prepare))
     (build (linux-cmem unpack)
          (linux install))
     (install (linux-cmem build)))

(pkg syslink
     (unpack (rootfs prepare))
     (build (syslink unpack)
          (linux install))
     (install (syslink build)))

(pkg chibi-scheme
     (config host
             (unpack)
             (build (chibi-scheme unpack host))
             (install (chibi-scheme build host)))
     (unpack)
     (build (chibi-scheme unpack)
          (chibi-scheme install host))
     (install (chibi-scheme build)))

(pkg chicken
     (unpack)
     (prepare (chicken unpack))
     (build (chicken prepare)
          (after (chicken install cross)))
     (install (chicken build)
              (rootfs prepare))
     (config host
             (build (chicken prepare))
             (install (chicken build host)))
     (config cross
             (build (chicken prepare)
                  (after (chicken install host)))
             (install (chicken build cross))))

(pkg libuv
     (config host
             (unpack)
             (prepare (libuv unpack host))
             (build (libuv prepare host))
             (install (libuv build host)))
     (unpack)
     (prepare (libuv unpack))
     (build (libuv prepare) (libuv install host))
     (install (libuv build) (rootfs prepare)))

(pkg luajit
     (config host
             (unpack)
             (build (luajit unpack host))
             (install (luajit build host)))
     (unpack)
     (build (luajit unpack) (luajit install host))
     (install (luajit build) (rootfs prepare)))

(pkg ffmpeg
     (unpack)
     (build (ffmpeg unpack))
     (install (ffmpeg build) (rootfs prepare))
     (config host
             (build (ffmpeg unpack))
             (install (ffmpeg build host))))

(pkg soundtouch
     (unpack)
     (prepare (soundtouch unpack))
     (build (soundtouch prepare))
     (install (soundtouch build) (rootfs prepare)))
