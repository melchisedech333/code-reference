
# Git

Language: <a href="readme-pt.md">PT-BR</a>

<br>

<b>Table of contents</b>
- Identity
- Credentials
- Help
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

## Help

```bash
git help <verb>
```

Example:
```bash
git help config
```

<br>

***

## Refences

- https://git-scm.com/book


