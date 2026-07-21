verify:
  uv run tools/verify_jekyll.py

zip:
	uv run tools/zip_it.py course-files/exercises
	uv run tools/zip_it.py course-files/lectures
	uv run tools/zip_it.py course-files/tutorials

serve-search: build
	bundle exec jekyll serve

build: verify zip
  JEKYLL_ENV=production bundle exec jekyll build --trace
  uv run tools/custom_indexer.py

serve:
	bundle exec jekyll serve

verify-solutions:
	#!/usr/bin/env bash
	cd ../autograder
	./tests/run-autograder.rkt cs111-testing --version bain --prefix tests/grading- tests/test_students.json -- tutorial-tests/tutorial*tests*.rkt ../tutorial_solutions/tut*sol*/tutorial*sol*.rkt
	./tests/validate-grades.rkt tests/grading-grades.json tutorial-tests/tutorial*tests*.rkt ../tutorial_solutions/tut*sol*/tutorial*sol*.rkt
	
	./tests/run-autograder.rkt cs111-testing --version bain --prefix tests/grading- tests/test_students.json -- exercise-tests/exercise*tests*.rkt ../exercise_solutions/ex*sol*/exercise*sol*.rkt
	./tests/validate-grades.rkt tests/grading-grades.json exercise-tests/exercise*tests*.rkt ../exercise_solutions/ex*sol*/exercise*sol*.rkt

test-deploy:
	uv run tools/canvas_deploy.py deploy --verbose

deploy:
	uv run tools/canvas_deploy.py deploy --no-dry-run

test-web: build
	bundle exec rake test

download-slides:
	uv run tools/download_slides.py

build-slides:
  quarto render slides/week*.qmd