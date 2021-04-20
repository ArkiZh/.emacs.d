
(defgroup arki/python nil
  "Python config."
  :group 'arki/config)


(defcustom arki/lsp-pyls-server-command nil
  "Python language server location.

May be here: {CONDA_HOME}/Scripts/pyls {PYTHON_ENV_HOME}/bin/pyls
"
  :group 'arki/python)


;; 配置lsp-mode
(add-hook 'python-mode-hook (lambda ()
			      (setq lsp-keymap-prefix "C-c l")  ;;Should be set before require
			      (require 'lsp-mode)
			      ;; Install pyls by: pip install 'python-language-server[all]' -i https://pypi.tuna.tsinghua.edu.cn/simple/
			      ;; Specify you pyls command path, before or after require
			      (if (or (not (boundp 'lsp-pyls-server-command)) (not (file-exists-p (car lsp-pyls-server-command))))
				  (progn 
				    (message "Select python language server: Windows:C:/DevSoft/anaconda/anaconda3_5.3.0/Scripts/pyls Mac:~/dev_tool/miniconda3/envs/ml/bin/pyls")
				    ;; (customize-save-variable 'lsp-pyls-server-command (list (read-file-name "Select your plys:")))
				    )
				)
			      ))
(add-hook 'python-mode-hook #'lsp-deferred)
;; 配置realgud，python的debug插件

(add-hook 'python-mode-hook (lambda () (load-library "realgud")))
;; 配置conda环境
(add-hook 'python-mode-hook (lambda ()
			      (if (or (not (boundp 'codna-anaconda-home)) (not (file-exists-p conda-anaconda-home)))
				  ;; A valid conda-anacodna-home Should be set before require, if not the default(~/.anaconda3)
				  (progn
				    (message "Select conda home: Windows:C:/DevSoft/anaconda/anaconda3_5.3.0 Mac:~/dev_tool/miniconda3")
				    ;; (customize-save-variable 'conda-anaconda-home (read-file-name "Select your conda home:"))
				    )
				)
			      (require 'conda)
			      (conda-env-initialize-interactive-shells)  ;;interactive shell support
			      (conda-env-initialize-eshell)  ;;eshell support
			      ;;(conda-env-autoactivate-mode t)  ;;try and detect the correct conda environment for a buffer,automatically
			      ))


;; 配置anaconda-mode
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))
;; (add-hook 'python-mode-hook 'anaconda-mode)
