;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (guix packages)
             (guix download)
             (guix gexp)
             (guix git-download)
             (guix build-system trivial)
             (gnu packages linux)
             (gnu)
             (secret))
(use-service-modules desktop networking nfs ssh xorg virtualization)

(define-public linux-firmware
  (let ((commit "711d3297bac870af42088a467459a0634c1970ca"))
    (package
      (name "linux-firmware")
      (version (string-append "2019.09.23-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                        (url (string-append
                              "https://github.com/wkennington/linux-firmware"))
                        (commit commit)))
                (sha256
                 (base32
                  "0dbnib2gh8kajyys3pps99lp5s4k6b26v2jmbq2x3vbl9fa1xgda"))))
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
      (home-page "https://github.com/wkennington/linux-firmware")
      (synopsis "Linux firmware")
      (description "Linux firmware.")
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
              (uuid secret-device-uuid))
            (target "cryptroot")
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
           %base-file-systems))
  (host-name "olimar")
  (users (cons* (user-account
                  (name "steve")
                  (comment "Stephen Webber")
                  (group "users")
                  (home-directory "/home/steve")
                  (supplementary-groups
                   '("wheel" "netdev" "audio" "video")))
                secret-account
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs"))
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
                (keyboard-layout keyboard-layout))))
      %desktop-services)))
