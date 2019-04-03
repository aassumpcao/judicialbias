import setuptools

# meta-data
NAME            = 'tjsp'
DESCRIPTION     = 'Package used to scrape and parse decisions from TJ-SP'
URL             = 'https://github.com/abjur/uncabj'
EMAIL           = 'andre.assumpcao@gmail.com'
AUTHOR          = 'Andre Assumpcao'
REQUIRES_PYTHON = '>=3.6.0'
VERSION         = '0.1'

# required packages
REQUIRED = [ 'codecs', 'glob', 'os', 'pandas', 're', 'time', 'bs4', 'selenium']

# optional packages
# EXTRAS = {'numpy', 'math'}

# load __version__.py
about = {}
if not VERSION:
    project_slug = NAME.lower().replace("-", "_").replace(" ", "_")
    with open(os.path.join(here, project_slug, '__version__.py')) as f:
        exec(f.read(), about)
else:
    about['__version__'] = VERSION

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name = NAME,
    version = about['__version__'],
    description = DESCRIPTION,
    long_description = long_description,
    long_description_content_type = 'text/markdown',
    author = AUTHOR,
    author_email = EMAIL,
    python_requires = REQUIRES_PYTHON,
    url = URL,
    # packages = find_packages(exclude = ('tests')),
    py_modules = ['tjsp'],
    install_requires = REQUIRED,
    # extras_require = EXTRAS,
    include_package_data = True,
    classifiers = [
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy'
    ],
)
