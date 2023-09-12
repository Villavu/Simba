from datetime import datetime

project = 'Simba'
author = 'Simba'
copyright = str(datetime.now().year) + ', Simba'
source_suffix = '.rst'
master_doc = 'index'
highlight_language = 'pascal'
html_title = 'Simba'
html_favicon = '../images/icon.ico'
html_theme = 'furo'
html_css_files = ['custom.css']
extensions = [
    'sphinx.ext.githubpages',
    'sphinx.ext.mathjax'
]
