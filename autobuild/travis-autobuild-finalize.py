import os
from git import git_delete_release, git_edit_release, git_make_release_data

if os.environ['TRAVIS_EVENT_TYPE'] == 'push':
    git_delete_release(os.environ['TRAVIS_BRANCH'] + '-autobuild')
    git_edit_release('autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'], git_make_release_data('autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'], os.environ['TRAVIS_BRANCH'], os.environ['TRAVIS_BRANCH'] + '-autobuild', 'Auto build for the ' + os.environ['TRAVIS_BRANCH'] + ' branch', 'false', 'true'))