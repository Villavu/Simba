import os
from git import git_upload_to_release

if os.environ['TRAVIS_EVENT_TYPE'] == 'push':
   for file in os.listdir(os.getcwd()):
       if file.startswith("Simba."):
	       git_upload_to_release('autobuild-' + os.environ['TRAVIS_BUILD_NUMBER'], os.path.join(os.getcwd(), file))