
# Git

<b>Table of contents</b>
- Identity
- Credentials
- General commands
- Add files, create commit, and send modifications
- Branch
- Synchronize files
- Merge
- Pull request
- References

<br>

***

## Identity


```bash
git config --global user.name "Nickname"
git config --global user.email example@exemplo.br
```

<br>

***

## Credentials

```bash
git config --global credential.helper store
```

<b>Note:</b> When you clone a repository, and run the push command, you will be prompted for credentials only once.

<b>Note:</b> As for Github, you must first generate an Access Token, currently at: <i>Settings > Developer settings > Personal access tokens</i>.

<br>

***

## General commands

```bash
# Help command...
git help <verb>
git help config

# Status...
git status
git log
git log --oneline --decorate

# Get branch list
git branch 
```

<br>

***

## Add files, create commit, and send modifications

```bash
git add .
git commit -m "Updates"
git push
```

<br>

***

## Branch

Create new branch:

```bash
git branch name_of_branch
```

<br>

Switching Branches:

```bash
git checkout name_of_branch
```

<br>

Create a new branch and switch:

```bash
git checkout -b name_of_branch
```

<br>

Send local branch to remote:

```bash
git add .
git commit -m "Updates"
git push --set-upstream origin name_of_branch
```

<br>

Rename branch:

```bash
git branch --move name_of_old_branch name_of_new_branch
git push --set-upstream origin name_of_new_branch
```

<br>

Delete branch:

```bash
git push origin --delete name_of_branch
```

<br>

## Synchronize files

```bash
git pull

# or...

git pull origin name_of_branch
```

<br>

## Merge

Change to branch and apply the merge:

```bash
git checkout master
git merge name_of_branch
```

<br>
Merge conflicts:

```
<<<<<<< HEAD:index.html
<div id="footer">contact : email.support@github.com</div>
=======
<div id="footer">
 please contact us at support@github.com
</div>
>>>>>>> iss53:index.html
```

<br>

Change the part to one of the merge and then commit the changes:

```
<div id="footer">
 please contact us at support@github.com
</div>
```

Send modifications:
```bash
git add . ; git commit -m "Updates"; git push
```

<br>

## Pull Request

Steps:

- fork repository
- clone repo, and apply modifications
- Go to the cloned project settings, and send the Pull Request.

<br>

***

## Refences

- https://git-scm.com/book


