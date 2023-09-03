.PHONY: all stg prod clean check-webmentions show-state show-drafts

ifeq ($(EMACS),)
EMACS := emacs
endif

EMACSFLAGS := --batch -L ./contrib -L ./lisp

IMGS := avatar.png

WWW_IMGS := $(IMGS:%=www/img/%)

all stg: lisp/indie-org.sh.el res/style.css $(WWW_IMGS)
	test -d www || mkdir www
	test -d www/res || mkdir www/res
	cp -v res/*.pgp www/
	cp -v res/style.css www/res
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/publish nil))'
	find www -iname '*~' -exec rm '{}' ';'
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/check-mf2))'
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/send-webmentions nil))'

prod: lisp/indie-org.sh.el res/style.css
	test -d www || mkdir www
	test -d www/res || mkdir www/res
	cp -v res/style.css www/res
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/publish t))'
	find www -iname '*~' -exec rm '{}' ';'
	aws s3 cp --recursive www/ s3://indie-org.sh
	aws cloudfront create-invalidation --distribution-id E2RNM3GDBAO9TO --paths '/*'
	aws cloudfront create-invalidation --distribution-id EG80WKV4HBYQO --paths '/*'
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/send-webmentions t))'
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/send-posse-requests t))'

clean:
	rm -rf www posts/{h-feed,rss,sitemap}.org

check-webmentions:
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(let ((print-level nil) (print-length nil)) (toggle-debug-on-error) (iosh/check-webmentions))'

show-state:
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(iosh/show-state)'

show-drafts:
	$(EMACS) $(EMACSFLAGS) -l indie-org.sh.el --eval '(iosh/show-drafts)'

www/img/%:
	test -d www/img || mkdir -p www/img
	cp -rv img/$* $@
