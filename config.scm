;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (guix packages)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix build-system trivial)
 (gnu packages linux)
 (gnu packages package-management)
 (gnu packages databases)
 (gnu)
 (secret))

(use-service-modules
 desktop
 networking
 nfs
 ssh
 xorg
 ;; mcron
 virtualization
 databases)

(define-public linux-firmware
  (let ((commit "05789708b79b38eb0f1a20d8449b4eb56d39b39f"))
    (package
      (name "linux-firmware")
      (version (string-append "2019.09.23-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                        (url (string-append
                              "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware"))
                        (commit commit)))
                (sha256
                 (base32
                  "1692nw80zaxfq59f85xnagc3mbr4a72hyisv7ifqsq55dbdyj9y5"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))

           (let* ((source   (assoc-ref %build-inputs "source"))
                  (out      (assoc-ref %outputs "out"))
                  (firmware (string-append out "/lib/firmware")))
             (mkdir-p firmware)
             (copy-recursively source firmware)))))
      (home-page "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git/about")
      (synopsis "Linux firmware")
      (description "Linux firmware")
      (license #f))))

(define-public linux
  (package
    (inherit linux-libre)
    (version "5.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cdn.kernel.org/pub/linux/kernel/v5.x/"
                    "linux-" version ".tar.xz"))
              (sha256
               (base32
                "0nzgkg4si0378pz6cv3hwj7qmmi5wdz1qvml0198b61n89xdcypc"))))))

(define steve-account
  (user-account
    (name "steve")
    (comment "Stephen Webber")
    (group "users")
    (home-directory "/home/steve")
    (supplementary-groups
     '("wheel" "netdev" "audio" "video"))))

(define accounts
  (list steve-account
        secret-account))

(define guix-gc-job #~(job '(next-hour '(4)) "guix gc -F 16G"))

(define daily-guix-pull-jobs
  (map (lambda (account) `#~(job '(next-hour '(2))
                                 (invoke "guix" "pull")
                                 #:user ,(user-account-name account)))
       accounts))

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (kernel linux)
  (firmware (list linux-firmware))
  (mapped-devices
    (list (mapped-device
            (source
              (uuid secret-root-uuid))
            (target "cryptroot")
            (type luks-device-mapping))
          (mapped-device
            (source
              (uuid secret-home-uuid))
            (target "crypthome")
            (type luks-device-mapping))))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid secret-boot-uuid 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device "/dev/mapper/cryptroot")
             (type "ext4")
             (dependencies mapped-devices))
           (file-system
             (mount-point "/home")
             (device "/dev/mapper/crypthome")
             (type "ext4")
             (dependencies mapped-devices))
           ;; TODO: doesn't mount automatically
           (file-system
             (mount-point "/mnt/media")
             (device (string-append "//" secret-server-ip "/media"))
             (type "cifs")
             (options (string-append "username=" secret-media-username ",password=" secret-media-password))
             (mount? #f)
             (create-mount-point? #t))
           %base-file-systems))
  (host-name "olimar")
  (users (append accounts %base-user-accounts))
  (packages
    (append
     (list
      (specification->package "nss-certs")
      postgresql)
     %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (service tor-service-type)
            (service qemu-binfmt-service-type
             (qemu-binfmt-configuration
              (platforms (lookup-qemu-platforms "arm" "aarch64" "mips64el"))
              (guix-support? #t)))
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            ;; TODO: fix mcron
            ;; (service mcron-service-type
            ;;  (mcron-configuration
            ;;   (jobs (cons*
            ;;          guix-gc-job
            ;;          daily-guix-pull-jobs))))
            ;; development
            (service redis-service-type)
            (service memcached-service-type)
            (service postgresql-service-type
                     (postgresql-configuration
                      (postgresql postgresql-13))))
      %desktop-services)))
