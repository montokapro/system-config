(use-modules (gnu)
             ;; (gnu packages databases)
             (montokapro databases))
(use-service-modules databases)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout
   (keyboard-layout "us" "altgr-intl"))

  ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
  ;; target hard disk, and "my-root" is the label of the target
  ;; root file system.
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sdX")))
  (file-systems
   (cons (file-system
          (device (file-system-label "my-root"))
          (mount-point "/")
          (type "ext4"))
         %base-file-systems))

  (host-name "postgres")

  (users
   (cons (user-account
          (name "app")
          (comment "app")
          (group "users")
          (supplementary-groups '("wheel")))
         %base-user-accounts))

  ;; ;; Globally-installed packages.
  ;; (packages (const postgresql %base-packages))

  (services
   (cons
    (service postgresql-service-type
             (postgresql-configuration
              (postgresql postgresql-ivm)))
    %base-services)))
