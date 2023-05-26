#!/bin/sh

# TODO: Use SourceHut Builds to update the README. Until then...

# Use the following post-commit hook:
# #!/bin/sh
# # When master branch is checked out
# [ $(git symbolic-ref --short -q HEAD) = "master" ] && \
# # And README.org was modified in last commit
# git diff --name-only HEAD^ | grep README.org && \
# # Run update-readme.sh
# cd ../../ && ./update-readme.sh

# TODO: Consider replacing this shell script with elisp like
# https://www.tomsdiner.org/blog/post_0003_sourcehut_readme_org_export.html

# Build README.html
emacs -Q --script ./build-readme.el
# Upload README.html
# Instructions for uploading custom README file copied from
# https://man.sr.ht/git.sr.ht/#setting-a-custom-readme
repo_id=265429
token=$(pass sourcehut/ushin-token)
readme=~/.local/src/hyperdrive.el/README.html

jq -sR '{
    "query": "mutation UpdateRepo($id: Int!, $readme: String!) {
      updateRepository(id: $id, input: { readme: $readme }) { id }
    }", "variables": {
      "id": '$repo_id',
      "readme": .
    } }' < $readme \
        | curl --oauth2-bearer $token \
               -H "Content-Type: application/json" \
               -d@- https://git.sr.ht/query

# Delete README.html
rm -f README.html
