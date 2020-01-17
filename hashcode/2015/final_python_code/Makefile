
envtmpdir=doc/build

.PHONY: tests, doc

check:
	python setup.py test

install:
	pip install .

clean:
	python setup.py clean
	find . -name "*.pyc" -delete
	find . -name "__pycache__" -delete
	rm -frv *.egg
	rm -frv hashcode_final.egg-info
	rm -frv dist
	rm -frv .tox
	rm -frv build
	rm -frv htmlcov
