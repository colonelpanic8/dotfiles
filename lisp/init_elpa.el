;; -------------------------------------------------------------------------
;; File: init-elpa.el - initialize the Emacs Lisp Package Archive (ELPA) 
;;                      see http://tromey.com/elpa/  
;;                      Install a base set of packages automatically.         
;;
;; Copyright (c) 2010 Sebastien Varrette <Sebastien.Varrette@uni.lu>
;;               http://varrette.gforge.uni.lu
;; 
;;          _       _ _             _                        _ 
;;         (_)_ __ (_) |_       ___| |_ __   __ _        ___| |
;;         | | '_ \| | __|____ / _ \ | '_ \ / _` |      / _ \ |
;;         | | | | | | ||_____|  __/ | |_) | (_| |  _  |  __/ |
;;         |_|_| |_|_|\__|     \___|_| .__/ \__,_| (_)  \___|_|
;;                                   |_|                       
;; 
;; More information about Emacs Lisp: 
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; -------------------------------------------------------------------------
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------------
;; Directly Inspired by Emacs Starter Kit 
;; see http://github.com/technomancy/emacs-starter-kit/blob/master/


;; Adapt to suit you needs
(defvar starter-kit-packages (list 'magit 'mo-git-blame 'python-mode)
  "Libraries that should be installed by default.")



(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (starter-kit-elpa-install))

;; Workaround for an ELPA bug that people are reporting but I've been
;; unable to reproduce:
(autoload 'paredit-mode "paredit")


(provide 'init-elpa)
