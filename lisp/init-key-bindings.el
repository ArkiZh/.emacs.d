;; 设置常用快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 配置counsel
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c p f") 'counsel-git) ;;查找当前所在git仓库管理的文件

;;配置org-mode中的快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

;; 将自定义函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

;; 将自定义函数 indent-region-or-buffer 绑定到 indent-region默认的<C-M-\> 键上
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

;; 使用hippie-expand增强自动补全
(global-set-key (kbd "M-RET") 'hippie-expand)

;; 使dired模式下按回车不会生成多个buffer
(with-eval-after-load 'dired 
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; 选择当前行
(global-set-key (kbd "C-q") 'select-current-line)

;; 打开treemmacs导航
(global-set-key (kbd "M-0") 'treemacs-select-window)

;; 配置ace-windown
(global-set-key (kbd "M-p") 'ace-window)

;; 配置move-text. Use default bindings for move-text-up and move-text-down (M-up / M-down).
;; (move-text-default-bindings)
(global-set-key [M-down] 'move-text-down)
(global-set-key [M-up]   'move-text-up)

;; 配置magit
(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-key-bindings)
