# counsel-gtags.el [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[GNU GLOBAL](https://www.gnu.org/software/global/) interface of [ivy](https://github.com/abo-abo/swiper).

## Tasks

- [X] Basic commands
- [X] find file command
- [X] Tag command
- [ ] Context command(dwim)
 - [X] Find definition and references
 - [ ] include header support
- [ ] `GTAGSLIBPATH` support
- [X] Basic History command
- [ ] History navigate command
- [ ] Tramp support
- [ ] Windows support

## Installation

counsel-gtags is not registered MELPA yet.

## Basic Usage

#### counsel-gtags-find-definition

Move to definition

Move command push current position to stack. `counsel-gtags-pop` command pops point stack and jump back to previous point.

#### counsel-gtags-find-reference

Move to references

#### counsel-gtags-find-symbol

Move to symbol references

#### counsel-gtags-find-file

Find file from tagged files

#### counsel-gtags-pop

Move to previous point on stack

#### counsel-gtags-create-tags

Create GNU GLOBAL tag

#### counsel-gtags-update-tags

Update tags.

#### counsel-gtags-dwim

Find name by context.

- Jump to tag definition if cursor is on tag reference
- Jump to tag reference if cursor is on tag definition

## Sample Configuration

```lisp
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop-stack))
```

[melpa-link]: https://melpa.org/#/counsel-gtags
[melpa-stable-link]: https://stable.melpa.org/#/counsel-gtags
[melpa-badge]: https://melpa.org/packages/counsel-gtags-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/counsel-gtags-badge.svg
