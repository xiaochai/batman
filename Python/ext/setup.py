from setuptools import setup, Extension

setup(
    name = 'palindrome1',
    version = '1.0',
    ext_modules = [
        Extension('palindrome1', ['palindrome1.c'])
    ]
)