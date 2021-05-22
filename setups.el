(defun sisci-workspace ()
  (interactive)
  ;; connect to tegra
  (vterm "tegra-vterm")
  (vterm-insert "ssh tegra-2")
  (vterm-send-return)
  ;; connect to x86
  (vterm "x86-vterm")
  (vterm-insert "ssh x86-1")
  (vterm-send-return)
  ;; setup windows
  (bookmark-jump "tegra-machine")
  (bookmark-jump "in5050-sisci")
  (split-window-right)
  (switch-to-buffer-other-window "x86-vterm")
  (split-window-below)
  (switch-to-buffer "tegra-vterm")
  (other-window 2)
  ;; set compilation
  (setq compile-command "./run.sh --tegra tegra-2 --clean")
  ;; store window setup for easier jumping
  (window-configuration-to-register 1)
  ;; Completion settings
  (add-to-list 'company-c-headers-path-system "/ssh:in5050:/usr/include/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/dis/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/os/"))

