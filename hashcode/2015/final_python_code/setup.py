#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import os.path
from pip.req import parse_requirements
import uuid
try:
    from setuptools import setup, find_packages
    from setuptools.command.test import test as TestCommand
except ImportError:
    from distutils.core import setup

VERSION = "0.0.1"


def get_local_file(path):
    """ Read content of local file """
    content = ''
    with open(path, 'r') as inpute_file:
        content = inpute_file.read()
    return content


def get_requirements(path='requirements.txt'):
    """ Read requirements from file """
    return [unicode(ir.req)
            for ir in parse_requirements(path, session=uuid.uuid1())]


class Tox(TestCommand):
    """ Run tox from setup.py """
    def finalize_options(self):
        TestCommand.finalize_options(self)
        self.test_args = []
        self.test_suite = True
    def run_tests(self):
        #import here, cause outside the eggs aren't loaded
        import tox
        errno = tox.cmdline(self.test_args)
        sys.exit(errno)


setup(
    # Project informations
    name='hashcode_final',
    version=VERSION,
    author=u"Les Geeks d'Orléans",
    author_email=u'remi.berson@gmail.com',
    download_url='https://bitbucket.org/remusao/hash-code',

    # License and description
    license=get_local_file("LICENSE"),
    description=u'42',
    long_description=(get_local_file("README.md")
                    + '\n'
                    + get_local_file(os.path.join("docs", "HISTORY.txt"))),
    # Package, scripts informations
    packages=find_packages(exclude=['ez_setup']),
    classifiers=[
        'Environment :: Console',
        'Programming Language :: Python :: 2.7',
        'License :: Other/Proprietary License'
    ],
    install_requires=get_requirements(),
    entry_points={
        'console_scripts': [
            'hcfinal = hashcode.main:main'
        ]
    },

    # Tests
    tests_require=get_requirements('test-requirements.txt'),
    cmdclass={'test': Tox},
    # Magic !
    zip_safe=False
)
