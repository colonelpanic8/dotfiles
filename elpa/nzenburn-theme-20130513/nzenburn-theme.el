;;; nzenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2013 Bozhidar Batsov

;; Author: nopcall <nopcall@gmail.com>
;; URL: http://github.com/nopcall/nopcall-nzenburn
;; Version: 20130513
;; X-Original-Version: 2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Vim theme Nzenburn for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Jani Nurminen created the original theme for vim on such this port
;; is based.

;;; Code:
(deftheme nzenburn "The Nzenburn color theme")

(let ((class '((class color) (min-colors 89)))
      ;; nzenburn palette
      ;; colors with +x are lighter, colors with -x are darker
      (nzenburn-fg "#dcdccc")
      (nzenburn-fg-1 "#656555")
      (nzenburn-bg-1 "#2b2b2b")
      (nzenburn-bg-05 "#383838")
      (nzenburn-bg "#3f3f3f")
      (nzenburn-bg+1 "#4f4f4f")
      (nzenburn-bg+2 "#5f5f5f")
      (nzenburn-bg+3 "#6f6f6f")
      (nzenburn-red+1 "#dca3a3")
      (nzenburn-red "#cc9393")
      (nzenburn-red-1 "#bc8383")
      (nzenburn-red-2 "#ac7373")
      (nzenburn-red-3 "#9c6363")
      (nzenburn-red-4 "#8c5353")
      (nzenburn-orange "#dfaf8f")
      (nzenburn-yellow "#f0dfaf")
      (nzenburn-yellow-1 "#e0cf9f")
      (nzenburn-yellow-2 "#d0bf8f")
      (nzenburn-green-1 "#5f7f5f")
      (nzenburn-green "#7f9f7f")
      (nzenburn-green+1 "#8fb28f")
      (nzenburn-green+2 "#9fc59f")
      (nzenburn-green+3 "#afd8af")
      (nzenburn-green+4 "#bfebbf")
      (nzenburn-cyan "#93e0e3")
      (nzenburn-blue+1 "#94bff3")
      (nzenburn-blue "#8cd0d3")
      (nzenburn-blue-1 "#7cb8bb")
      (nzenburn-blue-2 "#6ca0a3")
      (nzenburn-blue-3 "#5c888b")
      (nzenburn-blue-4 "#4c7073")
      (nzenburn-blue-5 "#366060")
      (nzenburn-magenta "#dc8cc3")
      (nzenburn-violet  "#6c71c4")
      (nzenburn-region  "#7f073f")
      )
  (custom-theme-set-faces
   'nzenburn
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,nzenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,nzenburn-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((t (:foreground ,nzenburn-fg :background ,nzenburn-bg))))
   `(cursor ((t (:foreground ,nzenburn-fg :background "white"))))
   `(escape-glyph ((t (:foreground ,nzenburn-yellow :bold t))))
   `(fringe ((t (:foreground ,nzenburn-fg :background ,nzenburn-bg+1))))
   `(header-line ((t (:foreground ,nzenburn-yellow
                                  :background ,nzenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,nzenburn-bg-05))))
   `(success ((t (:foreground ,nzenburn-green :weight bold))))
   `(warning ((t (:foreground ,nzenburn-orange :weight bold))))

   ;;; compilation
   `(compilation-column-face ((t (:foreground ,nzenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,nzenburn-green))))
   `(compilation-error-face ((t (:foreground ,nzenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,nzenburn-fg))))
   `(compilation-info-face ((t (:foreground ,nzenburn-blue))))
   `(compilation-info ((t (:foreground ,nzenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,nzenburn-green))))
   `(compilation-line-face ((t (:foreground ,nzenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,nzenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,nzenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,nzenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,nzenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,nzenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,nzenburn-yellow :weight bold))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,nzenburn-fg))))
   `(grep-error-face ((t (:foreground ,nzenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,nzenburn-blue))))
   `(grep-match-face ((t (:foreground ,nzenburn-orange :weight bold))))
   `(match ((t (:background ,nzenburn-bg-1 :foreground ,nzenburn-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((t (:foreground ,nzenburn-yellow-2 :weight bold :background ,nzenburn-bg-1))))
   `(isearch-fail ((t (:foreground ,nzenburn-fg :background ,nzenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,nzenburn-yellow-2 :weight bold :background ,nzenburn-bg-05))))

   `(menu ((t (:foreground ,nzenburn-fg :background ,nzenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,nzenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,nzenburn-green+1
                           :background ,nzenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,nzenburn-green-1
                      :background ,nzenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,nzenburn-region))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,nzenburn-orange))))
   `(trailing-whitespace ((t (:background ,nzenburn-red))))
   `(vertical-border ((t (:foreground ,nzenburn-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,nzenburn-cyan))))
   `(font-lock-comment-face ((t (:foreground ,nzenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,nzenburn-green))))
   `(font-lock-constant-face ((t (:foreground ,nzenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,nzenburn-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,nzenburn-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,nzenburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,nzenburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,nzenburn-blue+1))))
   `(font-lock-string-face ((t (:foreground ,nzenburn-red))))
   `(font-lock-type-face ((t (:foreground ,nzenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,nzenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,nzenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,nzenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,nzenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,nzenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,nzenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,nzenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,nzenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,nzenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,nzenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,nzenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,nzenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,nzenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,nzenburn-yellow))))

   ;;; external
   `(ace-jump-face-background
     ((t (:foreground ,nzenburn-fg-1 :background ,nzenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,nzenburn-green+2 :background ,nzenburn-bg :inverse-video nil))))

   ;; full-ack
   `(ack-separator ((t (:foreground ,nzenburn-fg))))
   `(ack-file ((t (:foreground ,nzenburn-blue))))
   `(ack-line ((t (:foreground ,nzenburn-yellow))))
   `(ack-match ((t (:foreground ,nzenburn-orange :background ,nzenburn-bg-1 :weight bold))))

   ;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,nzenburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((t (:background ,nzenburn-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,nzenburn-blue-4 :foreground ,nzenburn-fg))))
   `(popup-tip-face ((t (:background ,nzenburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,nzenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,nzenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,nzenburn-bg :foreground ,nzenburn-fg))))

   ;; android mode
   `(android-mode-debug-face ((t (:foreground ,nzenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,nzenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,nzenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,nzenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,nzenburn-yellow))))

   ;; bm
   `(bm-face ((t (:background ,nzenburn-yellow-1 :foreground ,nzenburn-bg))))
   `(bm-fringe-face ((t (:background ,nzenburn-yellow-1 :foreground ,nzenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,nzenburn-green-1 :foreground ,nzenburn-bg))))
   `(bm-persistent-face ((t (:background ,nzenburn-green-1 :foreground ,nzenburn-bg))))

   ;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,nzenburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,nzenburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,nzenburn-green+1 :weight bold :underline t))))

   ;; ctable
   `(ctbl:face-cell-select ((t (:background ,nzenburn-blue :foreground ,nzenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,nzenburn-bg-05 :foreground ,nzenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,nzenburn-cyan :foreground ,nzenburn-bg))))

   ;; diff
   `(diff-added ((,class (:foreground ,nzenburn-green+4 :background nil))
                 (t (:foreground ,nzenburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,nzenburn-yellow))))
   `(diff-removed ((,class (:foreground ,nzenburn-red :background nil))
                   (t (:foreground ,nzenburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,nzenburn-bg+2))
                  (t (:background ,nzenburn-fg :foreground ,nzenburn-bg))))
   `(diff-file-header
     ((,class (:background ,nzenburn-bg+2 :foreground ,nzenburn-fg :bold t))
      (t (:background ,nzenburn-fg :foreground ,nzenburn-bg :bold t))))

   ;; dired+
   `(diredp-display-msg ((t (:foreground ,nzenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,nzenburn-orange))))
   `(diredp-date-time ((t (:foreground ,nzenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,nzenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,nzenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,nzenburn-blue :background ,nzenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,nzenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,nzenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,nzenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,nzenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,nzenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,nzenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,nzenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,nzenburn-red))))
   `(diredp-link-priv ((t (:foreground ,nzenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,nzenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,nzenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,nzenburn-fg))))
   `(diredp-number ((t (:foreground ,nzenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,nzenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,nzenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,nzenburn-green-1))))
   `(diredp-symlink ((t (:foreground ,nzenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,nzenburn-magenta))))

   ;; ert
   `(ert-test-result-expected ((t (:foreground ,nzenburn-green+4 :background ,nzenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,nzenburn-red :background ,nzenburn-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,nzenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,nzenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,nzenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,nzenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,nzenburn-cyan :weight bold))))

   ;; flycheck
   `(flycheck-error-face ((t (:foreground ,nzenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning-face ((t (:foreground ,nzenburn-orange :weight bold :underline t))))

   ;; flymake
   `(flymake-errline ((t (:foreground ,nzenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline ((t (:foreground ,nzenburn-orange :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((t (:foreground ,nzenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect ((t (:foreground ,nzenburn-red-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,nzenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,nzenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,nzenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,nzenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,nzenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,nzenburn-green))))
   `(erc-pal-face ((t (:foreground ,nzenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,nzenburn-orange :background ,nzenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,nzenburn-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,nzenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,nzenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,nzenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,nzenburn-fg :weight bold :inverse-video t))))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,nzenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,nzenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,nzenburn-magenta :weight bold))))

   ;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,nzenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,nzenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,nzenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,nzenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,nzenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,nzenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,nzenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,nzenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,nzenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,nzenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,nzenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,nzenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,nzenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,nzenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,nzenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,nzenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,nzenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,nzenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,nzenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,nzenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,nzenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,nzenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,nzenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,nzenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,nzenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,nzenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,nzenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,nzenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,nzenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,nzenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,nzenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,nzenburn-yellow))))
   `(gnus-x ((t (:background ,nzenburn-fg :foreground ,nzenburn-bg))))

   ;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,nzenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,nzenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,nzenburn-green+1))))

   ;; helm
   `(helm-header
     ((t (:foreground ,nzenburn-green
                      :background ,nzenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,nzenburn-yellow
                      :background ,nzenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,nzenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,nzenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,nzenburn-bg :background ,nzenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,nzenburn-green+4 :background ,nzenburn-bg-1))))
   `(helm-ff-directory ((t (:foreground ,nzenburn-magenta))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,nzenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,nzenburn-bg-05)) ; old emacsen
              (t :weight bold)))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,nzenburn-bg+1))
                   (t :weight bold)))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,nzenburn-red :weight bold))))
   `(ido-only-match ((t (:foreground ,nzenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,nzenburn-yellow))))
   `(ido-indicator ((t (:foreground ,nzenburn-red :background ,nzenburn-bg))))

   ;; j2-mode
   `(js2-warning-face ((t (:underline ,nzenburn-orange))))
   `(js2-error-face ((t (:foreground ,nzenburn-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,nzenburn-green-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,nzenburn-green+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,nzenburn-green+3))))
   `(js2-function-param-face ((t (:foreground, nzenburn-green+3))))
   `(js2-external-variable-face ((t (:foreground ,nzenburn-orange))))

   ;; js3-mode
   `(js3-warning-face ((t (:underline ,nzenburn-yellow))))
   `(js3-error-face ((t (:foreground nil :underline ,nzenburn-red))))
   `(js3-external-variable-face ((t (:foreground ,nzenburn-magenta))))
   `(js3-function-param-face ((t (:foreground ,nzenburn-blue))))
   `(js3-jsdoc-tag-face ((t (:foreground ,nzenburn-magenta))))
   `(js3-jsdoc-type-face ((t (:foreground ,nzenburn-cyan))))
   `(js3-jsdoc-value-face ((t (:foreground ,nzenburn-violet))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground ,nzenburn-blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,nzenburn-green))))
   `(js3-instance-member-face ((t (:foreground ,nzenburn-blue))))
   `(js3-private-function-call-face ((t (:foreground ,nzenburn-red))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,nzenburn-red))))

   ;; RHTML
   `(erb-delim-face ((,class (:background ,nzenburn-bg+2))))
   `(erb-exec-face ((,class (:background ,nzenburn-bg+2 :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,nzenburn-bg+2))))
   `(erb-out-face ((,class (:background ,nzenburn-bg+2 :weight bold))))
   `(erb-out-delim-face ((,class (:background ,nzenburn-bg+2))))
   `(erb-comment-face ((,class (:background ,nzenburn-bg+2 :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,nzenburn-bg+2))))

   ;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,nzenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,nzenburn-blue-1))))
   `(jabbee-roster-user-dnd ((t (:foreground ,nzenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,nzenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,nzenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,nzenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,nzenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,nzenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((t (:foreground ,nzenburn-green+2 :background ,nzenburn-bg))))

   ;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,nzenburn-green+2 :background ,nzenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,nzenburn-red+1 :background ,nzenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,nzenburn-blue+1 :background ,nzenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,nzenburn-magenta :background ,nzenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,nzenburn-yellow :background ,nzenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))

   ;; magit
   `(magit-section-title ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,nzenburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,nzenburn-bg+1))))

   ;; egg
   `(egg-text-base ((t (:foreground ,nzenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,nzenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,nzenburn-green+3))))
   `(egg-branch ((t (:foreground ,nzenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,nzenburn-yellow))))
   `(egg-term ((t (:foreground ,nzenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,nzenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,nzenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,nzenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,nzenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,nzenburn-green+4))))

   ;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,nzenburn-green+1))))
   `(message-header-other ((t (:foreground ,nzenburn-green))))
   `(message-header-to ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,nzenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,nzenburn-green))))
   `(message-mml ((t (:foreground ,nzenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((t (:foreground ,nzenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,nzenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,nzenburn-green))))
   `(mew-face-header-to ((t (:foreground ,nzenburn-red))))
   `(mew-face-header-key ((t (:foreground ,nzenburn-green))))
   `(mew-face-header-private ((t (:foreground ,nzenburn-green))))
   `(mew-face-header-important ((t (:foreground ,nzenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,nzenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,nzenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,nzenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,nzenburn-red))))
   `(mew-face-body-url ((t (:foreground ,nzenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,nzenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,nzenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,nzenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,nzenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,nzenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,nzenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,nzenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,nzenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,nzenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,nzenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,nzenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,nzenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,nzenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,nzenburn-yellow))))

   ;; mic-paren
   `(paren-face-match ((t (:foreground ,nzenburn-cyan :background ,nzenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,nzenburn-bg :background ,nzenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,nzenburn-bg :background ,nzenburn-red :weight bold))))

   ;; mingus
   `(mingus-directory-face ((t (:foreground ,nzenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,nzenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,nzenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,nzenburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,nzenburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,nzenburn-red))))

   ;; nav
   `(nav-face-heading ((t (:foreground ,nzenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,nzenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,nzenburn-green))))
   `(nav-face-hdir ((t (:foreground ,nzenburn-red))))
   `(nav-face-file ((t (:foreground ,nzenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,nzenburn-red-4))))

   ;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,nzenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,nzenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,nzenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,nzenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,nzenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,nzenburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,nzenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,nzenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,nzenburn-bg+3 :strike-through t))))

   ;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,nzenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,nzenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,nzenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,nzenburn-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,nzenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,nzenburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,nzenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,nzenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,nzenburn-green+3))))
   `(org-formula ((t (:foreground ,nzenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,nzenburn-green+3))))
   `(org-hide ((t (:foreground ,nzenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,nzenburn-orange))))
   `(org-level-2 ((t (:foreground ,nzenburn-green+4))))
   `(org-level-3 ((t (:foreground ,nzenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,nzenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,nzenburn-cyan))))
   `(org-level-6 ((t (:foreground ,nzenburn-green+2))))
   `(org-level-7 ((t (:foreground ,nzenburn-red-4))))
   `(org-level-8 ((t (:foreground ,nzenburn-blue-4))))
   `(org-link ((t (:foreground ,nzenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,nzenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,nzenburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,nzenburn-blue+1))))
   `(org-special-keyword ((t (:foreground ,nzenburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,nzenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,nzenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,nzenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,nzenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,nzenburn-bg-1))))
   `(org-column-title ((t (:background ,nzenburn-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-1 ((t (:foreground ,nzenburn-orange))))
   `(outline-2 ((t (:foreground ,nzenburn-green+4))))
   `(outline-3 ((t (:foreground ,nzenburn-blue-1))))
   `(outline-4 ((t (:foreground ,nzenburn-yellow-2))))
   `(outline-5 ((t (:foreground ,nzenburn-cyan))))
   `(outline-6 ((t (:foreground ,nzenburn-green+2))))
   `(outline-7 ((t (:foreground ,nzenburn-red-4))))
   `(outline-8 ((t (:foreground ,nzenburn-blue-4))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,nzenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,nzenburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,nzenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,nzenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,nzenburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,nzenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,nzenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,nzenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,nzenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,nzenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,nzenburn-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,nzenburn-blue-5))))

   ;; MMM-mode
   `(mmm-code-submode-face ((t (:background ,nzenburn-bg+1))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background ,nzenburn-bg+1))))

   ;;rcirc
   `(rcirc-my-nick ((t (:foreground ,nzenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,nzenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,nzenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,nzenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,nzenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,nzenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,nzenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,nzenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,nzenburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,nzenburn-yellow :bold t))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,nzenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,nzenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,nzenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,nzenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,nzenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,nzenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,nzenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,nzenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,nzenburn-red))))

   ;; rst-mode
   `(rst-level-1-face ((t (:foreground ,nzenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,nzenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,nzenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,nzenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,nzenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,nzenburn-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((t (:foreground ,nzenburn-red-3 :background ,nzenburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,nzenburn-blue-1 :background ,nzenburn-bg :weight bold))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,nzenburn-red))))

   ;; tabbar
   `(tabbar-button ((t (:foreground ,nzenburn-fg
                                    :background ,nzenburn-bg))))
   `(tabbar-selected ((t (:foreground ,nzenburn-fg
                                      :background ,nzenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,nzenburn-fg
                                        :background ,nzenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))

   ;; term
   `(term-color-black ((t (:foreground ,nzenburn-bg
                                       :background ,nzenburn-bg-1))))
   `(term-color-red ((t (:foreground ,nzenburn-red-2
                                     :background ,nzenburn-red-4))))
   `(term-color-green ((t (:foreground ,nzenburn-green
                                       :background ,nzenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,nzenburn-orange
                                        :background ,nzenburn-yellow))))
   `(term-color-blue ((t (:foreground ,nzenburn-blue-1
                                      :background ,nzenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,nzenburn-magenta
                                         :background ,nzenburn-red))))
   `(term-color-cyan ((t (:foreground ,nzenburn-cyan
                                      :background ,nzenburn-blue))))
   `(term-color-white ((t (:foreground ,nzenburn-fg
                                       :background ,nzenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,nzenburn-bg-05))))

   ;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,nzenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,nzenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,nzenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,nzenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,nzenburn-green+2 :background ,nzenburn-bg))))
   `(w3m-lnum-match ((t (:background ,nzenburn-bg-1
                                     :foreground ,nzenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,nzenburn-yellow))))

   ;; whitespace-mode
   `(whitespace-space ((t (:background ,nzenburn-bg+1 :foreground ,nzenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,nzenburn-bg+1 :foreground ,nzenburn-bg+1))))
   `(whitespace-tab ((t (:background ,nzenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,nzenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,nzenburn-red))))
   `(whitespace-line ((t (:background ,nzenburn-bg :foreground ,nzenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,nzenburn-orange :foreground ,nzenburn-orange))))
   `(whitespace-indentation ((t (:background ,nzenburn-yellow :foreground ,nzenburn-red))))
   `(whitespace-empty ((t (:background ,nzenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,nzenburn-yellow :foreground ,nzenburn-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,nzenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,nzenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,nzenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,nzenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,nzenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,nzenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,nzenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,nzenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,nzenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,nzenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,nzenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,nzenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,nzenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,nzenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,nzenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,nzenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,nzenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,nzenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,nzenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,nzenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,nzenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,nzenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,nzenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,nzenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,nzenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((t (:foreground ,nzenburn-green+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,nzenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,nzenburn-bg-1 :foreground ,nzenburn-bg-1)))))

  ;;; custom theme variables
  (custom-theme-set-variables
   'nzenburn
   `(ansi-color-names-vector [,nzenburn-bg ,nzenburn-red ,nzenburn-green ,nzenburn-yellow
                                           ,nzenburn-blue ,nzenburn-magenta ,nzenburn-cyan ,nzenburn-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,nzenburn-bg-05)

   ;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,nzenburn-red-1)
       ( 40. . ,nzenburn-red)
       ( 60. . ,nzenburn-orange)
       ( 80. . ,nzenburn-yellow-2)
       (100. . ,nzenburn-yellow-1)
       (120. . ,nzenburn-yellow)
       (140. . ,nzenburn-green-1)
       (160. . ,nzenburn-green)
       (180. . ,nzenburn-green+1)
       (200. . ,nzenburn-green+2)
       (220. . ,nzenburn-green+3)
       (240. . ,nzenburn-green+4)
       (260. . ,nzenburn-cyan)
       (280. . ,nzenburn-blue-2)
       (300. . ,nzenburn-blue-1)
       (320. . ,nzenburn-blue)
       (340. . ,nzenburn-blue+1)
       (360. . ,nzenburn-magenta)))
   `(vc-annotate-very-old-color ,nzenburn-magenta)
   `(vc-annotate-background ,nzenburn-bg-1)
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nzenburn)


;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; nzenburn-theme.el ends here
