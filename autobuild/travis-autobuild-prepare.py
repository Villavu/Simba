import os
from git import git_create_release, git_make_release_data

if os.environ['TRAVIS_EVENT_TYPE'] == 'push':
    git_create_release(git_make_release_data('autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'], os.environ['TRAVIS_BRANCH'], 'autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'], 'Build in progress...', 'true', 'true'))