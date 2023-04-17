build:
	cabal run site

deploy:
	rsync -avz -e 'ssh' out/ chris-martin.org:/var/www/chris-martin.org/

.PHONY: build deploy
