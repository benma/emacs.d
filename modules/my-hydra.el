(require 'my-projects)
(require 'hydra)

(defhydra hydra-compilation-firmware_v2-jlink (:exit t)
  "jlink"
  ("f" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make firmware && make jlink-flash-firmware") "jlink firmware")
  ("b" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make bootloader && make jlink-flash-bootloader") "jlink bootloader"))

(defhydra hydra-compilation-firmware_v2 (:exit t)
  "firmware_v2"
  ("c" (projectile-compile-project t) "compile")
  ("F" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make firmware") "firmware")
  ("f" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make firmware && make flash-dev-firmware") "firmware/flash")
  ("b" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make bootloader") "bootloader")
  ("t" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make run-unit-tests") "unit tests")
  ("j" (hydra-compilation-firmware_v2-jlink/body) "jlink"))

(defhydra hydra-compilation-bitbox-wallet-app-locize (:exit t)
  "locize"
  ("p" (project-compilation-cmd-noninteractive "make locize-pull") "pull")
  ("P" (project-compilation-cmd-noninteractive "make locize-push") "push"))

(defhydra hydra-compilation-bitbox-wallet-app (:exit t)
  "bitbox-wallet-app"
  ("c" (projectile-compile-project t) "compile")
  ("s" (project-compilation-cmd-noninteractive "make servewallet") "servewallet")
  ("m" (project-compilation-cmd-noninteractive "make servewallet-mainnet") "servewallet-mainnet")
  ("C" (project-compilation-cmd-noninteractive "make ci") "CI")
  ("w" (project-compilation-cmd-noninteractive "make weblint") "weblint")
  ("l" (hydra-compilation-bitbox-wallet-app-locize/body) "locize"))

(provide 'my-hydra)
