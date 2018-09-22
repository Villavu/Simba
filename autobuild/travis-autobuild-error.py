import os
from git import git_delete_release

if os.environ['TRAVIS_EVENT_TYPE'] == 'push':
   git_delete_release('autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'])