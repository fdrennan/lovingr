snapshot:
	R -e "renv::snapshot"
	
restore:
	R -e "renv::restore()"
	
style: sass
	R -e "styler::style_dir()"

sass:
	sass www/styles.scss www/styles.css

push: style snapshot
	git add --all
	git commit -m 'stored and shit'
	git push origin $$(git rev-parse --abbrev-ref HEAD)


