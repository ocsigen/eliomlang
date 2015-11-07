# OASIS_START
# OASIS_STOP

NAME    = $(shell grep 'Name:' _oasis    | sed 's/Name: *//')
VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')

ARCHIVE = https://github.com/Drup/$(NAME)/archive/$(VERSION).tar.gz

# doc update

doc/html/.git:
	mkdir -p doc/html
	cd doc/html && (\
		git init && \
		git remote add origin git@github.com:Drup/$(NAME).git && \
		git checkout -b gh-pages \
	)

gh-pages: doc/html/.git
	cd doc/html && git checkout gh-pages
	rm -f doc/html/*.html
	cp $(NAME).docdir/*.html doc/html/
	cd doc/html && git add *.html
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages

# release

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push upstream $(VERSION)
	$(MAKE) pr
