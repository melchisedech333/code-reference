
# Git

<b>Table of contents</b>
- Identity
- Credentials
- Help and others
- Add files, create commit, and send modifications
- Branch
- Synchronize files
- Merge
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

## Help and others

```bash
git help <verb>
```

Example:
```bash
git help config
```

<br>

Other commands:

```bash
git status
git log
git log --oneline --decorate
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

## Synchronize files

```bash
git pull

# or...

git pull origin name_of_branch
```

<br>

## Merge

```bash

```

<br>

***

## Refences

- https://git-scm.com/book


