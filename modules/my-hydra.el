(require 'my-projects)
(require 'hydra)

(defhydra hydra-compilation-bitbox02-firmware-jlink (:exit t)
  "jlink"
  ("f" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j firmware && make jlink-flash-firmware") "jlink firmware")
  ("b" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j bootloader && make jlink-flash-bootloader") "jlink bootloader")
  ("d" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j bootloader-devdevice && make jlink-flash-bootloader") "jlink bootloader devdevice"))

(defhydra hydra-compilation-bitbox02-firmware-other (:exit t)
  "other"
  ("f" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh ./scripts/format") "format"))

(defhydra hydra-compilation-bitbox02-firmware (:exit t)
  "bitbox02-firmware"
  ("c" (projectile-compile-project t) "compile")
  ("F" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j firmware") "firmware")
  ("f" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j firmware && make flash-dev-firmware") "firmware/flash")
  ("b" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j bootloader") "bootloader")
  ("d" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh make -j bootloader-devdevice") "bootloaderd devdevice")
  ("t" (project-compilation-cmd-noninteractive "./scripts/docker_exec.sh 'make -j unit-test && make run-unit-tests'") "unit tests")
  ("j" (hydra-compilation-bitbox02-firmware-jlink/body) "jlink")
  ("o" (hydra-compilation-bitbox02-firmware-other/body) "other"))

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
