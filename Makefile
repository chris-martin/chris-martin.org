build:
	stack build --fast
	stack exec -- site

deploy:
	rsync -avz -e 'ssh -p 36411' out/ chris-martin.org:/var/www/chris-martin.org/

.PHONY: build deploy
