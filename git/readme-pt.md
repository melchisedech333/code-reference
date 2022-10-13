
# Git

Language: <a href="readme.md">EN-US</a>

<br>

<b>Sumário</b>
- Identificação
- Credenciais
- Ajuda
- Referências

<br>

***

## Identificação


```bash
git config --global user.name "Nickname"
git config --global user.email example@exemplo.br
```

<br>

***

## Credenciais

```bash
git config --global credential.helper store
```

<b>Obs:</b> ao clonar um repositório e executar o comando push, as credenciais serão solicitadas apenas uma vez.

<b>Obs:</b> se tratando do Github, você deve primeiro gerar um Access Token, atualmente você pode acessar a configuração por aqui: <i>Settings > Developer settings > Personal access tokens</i>.

<br>

***

## Ajuda

```bash
git help <verb>
```

Exemplo:
```bash
git help config
```

<br>

***

## Referências

- https://git-scm.com/book


