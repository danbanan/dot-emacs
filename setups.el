(defun sisci-workspace ()
  (interactive)
  ;; ;; connect to tegra
  ;; (vterm "tegra-vterm")
  ;; (vterm-insert "ssh tegra-2")
  ;; (vterm-send-return)
  ;; ;; connect to x86
  ;; (vterm "x86-vterm")
  ;; (vterm-insert "ssh x86-1")
  (vterm-send-return)
  ;; setup windows
  (bookmark-jump "tegra-2")
  (bookmark-jump "in5050-sisci")
  ;; (split-window-right)
  ;; (switch-to-buffer-other-window "x86-vterm")
  ;; (split-window-below)
  ;; (switch-to-buffer "tegra-vterm")
  ;; (other-window 2)
  ;; set compilation
  (setq compile-command "./run.sh --tegra tegra-2 --clean")
  ;; store window setup for easier jumping
  (window-configuration-to-register 1)
  ;; Completion settings
  (add-to-list 'company-c-headers-path-system "/ssh:in5050:/usr/include/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/dis/")
  (add-to-list 'company-c-headers-path-system "/ssh:tegra-2:/opt/DIS/include/os/")
  (setq company-gtags-executable "/ssh:in5050:/usr/bin/gtags"))

(defun sisci-decode ()
  (interactive)
  (compile "ssh in5050-2014-11 'ls -l ~/in5050-codec63-build/x86-build/output.c63;  ~/in5050-codec63/c63dec ~/in5050-codec63-build/x86-build/output.c63 ~/in5050-codec63/output.yuv; ls -l ~/in5050-codec63/output.yuv'"))

(defun sisci-predict ()
  (interactive)
  (compile "ssh in5050-2014-11 '~/in5050-codec63/c63pred ~/in5050-codec63-build/x86-build/output.c63 ~/in5050-codec63/output.yuv'"))

(defun sisci-play-foreman ()
  (interactive)
  (compile "ssh x86-2 'mplayer -demuxer rawvideo -rawvideo w=352:h=288 ~/in5050-codec63/output.yuv'"))

(defun sisci-play-tractor ()
  (interactive)
  (compile "ssh x86-2 'mplayer -demuxer rawvideo -rawvideo w=1920:h=1080 ~/in5050-codec63/output.yuv'"))


(defun sisci-decode1 ()
  (interactive)
  (compile "ssh in5050-2014-11 'ls -l ~/*.c63; ~/in5050-codec63/c63dec ~/output.c63 ~/in5050-codec63/output.yuv; ls -l ~/in5050-codec63/output.yuv'"))

(defun sisci-decode2 ()
  (interactive)
  (compile "ssh in5050-2014-11 'ls -l ~/*.c63; ~/in5050-codec63/c63dec ~/output2.c63 ~/in5050-codec63/output.yuv; ls -l ~/in5050-codec63/output.yuv'"))

(defun sisci-decode3 ()
  (interactive)
  (compile "ssh in5050-2014-11 'ls -l ~/*.c63; ~/in5050-codec63/c63dec ~/output3.c63 ~/in5050-codec63/output.yuv; ls -l ~/in5050-codec63/output.yuv'"))

(defun sisci-predict ()
  (interactive)
  (compile "ssh in5050-2014-10 '~/in5050-codec63/c63pred ~/in5050-codec63-build/x86-build/output.c63 ~/in5050-codec63/output.yuv'"))

(defun sisci-decode4 ()
  (interactive)
  (compile "ssh in5050-2014-11 'ls -l ~/*.c63; ~/in5050-codec63/c63dec ~/output4.c63 ~/in5050-codec63/output.yuv; ls -l ~/in5050-codec63/output.yuv'"))
