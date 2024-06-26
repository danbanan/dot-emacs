
(defvar air5-buffers (list "air5-1-exd_resmgr"
			   "air5-1-application"
			   "air5-3-exd_resmgr"
			   "air5-3-application"))

(defun 4x4-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally)
  (other-window 2))

(defun vterm-send-command (command)
  (when (eq major-mode 'vterm-mode)
    (vterm-send-string command)
    (vterm-send-return)))

(defun start-air5-testview ()
  (interactive)
  (4x4-layout)
  (save-window-excursion
    (dolist (bufname air5-buffers)
      (unless (get-buffer bufname)
	(vterm bufname)
	(pcase bufname
	  ("air5-1-exd_resmgr"
	   (vterm-send-command "ssh cariad-air5-1")
	   (vterm-send-command "cd /mnt/root/dan/artifacts/build")
	   (vterm-send-command "./driver/exd_resmgr_mx -n 1"))
	  ("air5-1-application"
	   (vterm-send-command "ssh cariad-air5-1")
	   (vterm-send-command "cd /mnt/root/dan/artifacts/build/examples")
	   (vterm-send-command (concat "export LD_LIBRARY_PATH="
				       (prin1-to-string "/root/dan/artifacts/build/libasisci/:$LD_LIBRARY_PATH"))))
	  ("air5-3-exd_resmgr"
	   (vterm-send-command "ssh cariad-air5-3")
	   (vterm-send-command "cd /mnt/root/dan/artifacts/build")
	   (vterm-send-command "./driver/exd_resmgr_mx -n 2"))
	  ("air5-3-application"
	   (vterm-send-command "ssh cariad-air5-3")
	   (vterm-send-command "cd /mnt/root/dan/artifacts/build/examples")
	   (vterm-send-command (concat "export LD_LIBRARY_PATH="
				       (prin1-to-string "/root/dan/artifacts/build/libasisci/:$LD_LIBRARY_PATH"))))))))
  (dolist (bufname air5-buffers)
    (switch-to-buffer bufname)
    (other-window 1))
  ;; Move to first application window
  (other-window 1))

(defun kill-air5-testview ()
  (interactive)
  (save-window-excursion
    (dolist (bufname air5-buffers)
      (when (get-buffer bufname)
	(with-current-buffer bufname
	  (let ((kill-buffer-query-functions nil))
	    (vterm-send-string "\C-c") ; Close current running process
	    (kill-buffer)))))))


