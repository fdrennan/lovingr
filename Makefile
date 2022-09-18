snapshot:
	R -e "renv::snapshot()"
	
restore:
	R -e "renv::restore()"
	
style: sass
	R -e "styler::style_dir()"

sass:
	sass www/styles.scss www/styles.css

push: 
	git add --all
	git commit -m 'lazy save'
	git push origin $$(git rev-parse --abbrev-ref HEAD)


all: style snapshot push
