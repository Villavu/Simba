import os
import json

def git(args):
    os.system("curl --silent -H 'Authorization: token " + os.environ['GITHUB_API_KEY'] + "' " + args + " 2>&1 | tee >/dev/null")

def git_get_releases():
    git('-o releases https://api.github.com/repos/' + os.environ['TRAVIS_REPO_SLUG'] + '/releases')

    with open('releases', 'r') as f:
        return json.loads(f.read()) 

def git_make_release_data(tag, commit, name, body, draft, prerelease):
    arg1 = '"tag_name": "' + tag + '"'
    arg2 = '"target_commitish": "' + commit + '"'
    arg3 = '"name": "' + name + '"'
    arg4 = '"body": "' + body + '"'
    arg5 = '"draft": ' + draft
    arg6 = '"prerelease": ' + prerelease

    return '{' + arg1 + ', ' + arg2 + ', ' + arg3 + ', ' + arg4 + ', ' + arg5 + ', ' + arg6 + '}' 

def git_create_release(release):
    git("-X POST --data '" + release + "' https://api.github.com/repos/" + os.environ['TRAVIS_REPO_SLUG'] + "/releases")

def git_upload_to_release(release_name, file_path):
    for release in git_get_releases():
        if release['name'] == release_name:
            head, tail = os.path.split(file_path)
            asset = 'https://uploads.github.com/repos/' + os.environ['TRAVIS_REPO_SLUG'] + '/releases/' + str(release['id']) + '/assets?name=' + tail
            git("-X POST --data-binary @'" + file_path + "' -H 'Content-Type: application/octet-stream' " + asset)
            break

def git_edit_release(release_name, data):
    for release in git_get_releases():
        if release['name'] == release_name:
            git("-X PATCH --data '" + data + "' https://api.github.com/repos/" + os.environ['TRAVIS_REPO_SLUG'] + "/releases/" + str(release['id']))

def git_delete_release(release_name):
    for release in git_get_releases():
       if release['name'] == release_name: 
           git("-X DELETE https://api.github.com/repos/" + os.environ['TRAVIS_REPO_SLUG'] + "/git/refs/tags/" + release['tag_name'])
           git("-X DELETE https://api.github.com/repos/" + os.environ['TRAVIS_REPO_SLUG'] + "/releases/" + str(release['id']))  

