# Signaux Faibles - Data Science Workflow

Le but de ce document est de formaliser un mode de travail permettant le développement et la mise en production rapide de nouveaux modèles pour le projet Signaux Faibles.

## Git Flow

Nous utilisons une version simplifiée du Git Flow:

### `main`
- `main` est la branche qui contient la seule et unique version du modèle signaux-faibles de production. En théorie, le modèle/code présent sur cette branche devrait pouvoir être utilisé pas n'importe qui (développeur, DevOps, data scientist) sans avoir à se concerter avec le data scientist responsable du modèle. Cette branche a donc vocation à être :
  - **stable** : seuls les modèles qui apportent une amélioration claire, documentée et testée sont mergés dans `main`. Cette branche contient du code fonctionnel et qui ne cassera pas la production.
  - **versionée** : les différentes releases sont taggées en suivant le format `MODELE.AMELIORATION.BUGFIX`.
  - **documentée** : un modèle utilisé en production doit nécéssairement avoir une documentation à jour. Il est également bon d'utiliser un [`changelog`](https://keepachangelog.com/en/1.0.0/) afin d'expliciter les changements apportés aux différentes versions du modèle.
  - **protégée** : il est impossible de pousser des changements directement dans `main`. Le seul moyen de la faire évoluer est de merger une pull request ayant été revue et validée par un.e pair.e — *idéalement* depuis `dev` (voir ci-dessous).

### `dev`
- `dev` est la branche sur laquelle les data scientists travaillent ensemble. Elle est moins stable que `main` et contient la prochaine release du modèle (dont elle peut être considerée comme une version "beta"). Une fois qu'assez de changements ont été apportés à `dev`, elle est mergée dans `main` (et taggée!). Cette branche doit être:
  - **_relativement_ stable**: cette branche est testée, la chaine d'intégration continue doit passer, le modèle qu'elle contient doit être utilisable et documenté.
  - **protégée** : il est impossible de pousser des changements directement dans `dev` — le seul moyen de la faire évoluer et de merger une pull request ayant été revue et validée par un.e pair.e — *idéalement* depuis les feature branches (voir ci-dessous).
  - **par défault** : le repo GitHub doit être configuré pour que la branche par défault du repo (pour les pull requests par exemple) soit `dev` et non pas `main`

### branches de `feature`
- `feat/<nom_de_la_feature>` sont les branches de travail pour les data scientists. Elle peuvent contenir des améliorations/évolutions du modèle, du code, des tests, etc. Elle doivent être:
  - **atomiques** et ne contenir _qu'une seule_ évolution par rapport à dev. Par exemple, `feat/better_hyperparam_tuning` se concentrera sur une amélioration du modèle existant alors que `feat/new_test_train_split` concernera une évolution des fonctions d'aides du repo. Il faut impérativement séparer ces deux chantiers sur deux branches différentes (quitte à travailler sur les 2 en parallèle) afin de pouvoir merger une branche indépendamment de l'autre.
  - **exploratoires** : une branche de feature est un espace de travail ou les data scientists peuvent casser et changer le code à loisir. NB: il est cependant recommandé de synchroniser régulièrement sa branche avec `dev` afin de bénéficier de certains changements déjà réalisés par d'autres
  - **éphémères** : une branche de feature n'est pas faite pour "stocker" du code sur de la longue durée, et a avant tout pour vocation d'être mergée dans `dev` sur le moyen terme. Cette rêgle peut être adaptée dans le cadre de travaux exploratoires sur des modèles de data science — auquel cas il serait bon de convenir d'une convention de nommage pour ces branches (par exemple, `modele/<model_name>`).

### `hotfix`
- `hotfix/<nom_du_fix>` est une branche dite de "hotfix". Elle peut être mergée directement en production (`main`) dans le cas ou cette dernière est cassée.
  - ne doit être ouverte que le temps de la résolution d'un bug en production
  - ne doit contenir que le code minimum nécessaire à la résolution du bug + le cas échéant, un test permettant de s'assurer que le bug ne réapparaitra pas
  - ne doit (évidemment) pas introduire de nouveaux bugs ou de régression, et doit donc à minima passer la CI/CD

### Resumé
Ce schéma résume notre git flow (NB: [l'utilisation du terme `main` est aujourd'hui préféré à celui, plus connoté, de `main`](https://www.techrepublic.com/article/github-to-replace-main-with-main-starting-in-october-what-developers-need-to-know/)):

![GitFlow simplifié](https://marcgg.com/assets/blog/git-flow-before.jpg)

(Les branches de `hotfix` partent et poussent dans `main`).


## Conventions de nommage

Le respect de certaines conventions de nommage permet de s'assurer de la bonne santée du projet.

### Branches
Pour une nouvelle feature
`feat/<name_of_feature>`

Pour un nouveau modèle
`model/<name_of_model>`

Pour une correction de bug en production
`hotfix/<description>`

### Commits
Un commit doit idéalement respecter ce format:

```
git commit -m '{TYPE}: MESSAGE'
```

- `{TYPE}`: decrit le type de commit;
  - _`fix`_ _bug fix_
  - _`feat`_ _rajout d'une feature_
  - _`enhance`_ _une amélioration (refacto, factorisation, simplification, sécurité, etc.)_
  - _`clean`_ _nettoyage du code_
  - _`doc`_ _ajout de documentation_
  - _`config`_ _changement de la configuration du projet_
  - _`test`_ _rajout ou mise-à-jour des tests_
  - _`M`_ _pour les changements majeurs (commit initial, nouveau modèle, résolution d'un gros bug)_

Il est possible de rajouter un BODY et un FOOTER à ses messages de commit 

```shell
git commit -m '{TYPE}: {MESSAGE}' -m '{BODY}' -m '{FOOTER}'
```

Qui peuvent servir a rajouter des information utiles à l'automatisation de taches par GitHub (par exemple, fermer des issues):

```shell
git commit -m 'feat: change test train proportion' \
 -m 'move value from .5 to .75' \
 -m 'closes #17 and #19'
```


## Outillage

Certains outils permettent de s'assurer de la pérénité et de la stabilité du code produit.

### Tests

L'outil standard pour tester en R est le package `assertthat`.

### Formattage de code

L'intérêt d'utiliser un outil de formattage automatique de code est qu'il permet d'assurer une cohérence au sein du repo en termes de style et de règles. Cela permet une meilleure compréhension du code, moins d'erreurs, et des revues de code plus rapides.

- le package [`styler`](https://github.com/r-lib/styler) semble être un bon outil


### Linter

Un linter s'assure que le code produit adhère aux bonnes pratiques de style et de syntaxe.

- le package [`lintr`](https://github.com/jimhester/lintr) est top et s'intègre bien avec `styler`

### Documentation

- `roxygen2`

### Git Hooks

Les git hooks permettent de s'assurer que certains scripts tournent automatiquement avant un `commit` ou un `push`. Ils sont stockés dans `.git/hooks/`

- `.git/hooks/pre-commit`
  - `styler` tous les fichiers
  - `lintr` tous les fichiers
- `.git/hooks/pre-push`
  - `test` et `build` le package R
  - faire tourner et évaluer le modèle (ça peut juste être un rappel si c'est trop lourd)


----

# Git Cheatsheet :construction_worker:

Quelques ressources (en anglais) sur Git

### Cloning the repository

If you are arriving on an existing project you will have to clone the git repository.

Again find the remote address of the repository and clone it:

```shell
git clone git@github.com:myusername/my-project.git
```

Navigate to the new cloned folder, list and pull all branches with their updates:

```shell
git branch -r
git fetch --all
git pull --all
```

Everything should be up to date.

>Don't forget to checkout on the current `dev` branch

### Creating and working on a branch

You should create a new branch for each major feature that you are developping.
To do so simply create a branch called `feat/<feature-name>` (where `feature-name` is the name of your feature) by running the commands:

```shell
# go to the dev branch and make sure you are up to date
git checkout dev
git pull

# create a new branch called feat/new_test_train_split
git checkout -b feat/new_test_train_split
git push -u origin feat/new_test_train_split
```

Once this is done run `git branch` to make sure that you are on the right branch.

### Commiting

Without doubt `git commit` is the most used git command during development. However, a lot of developper use it the wrong way so here's a set of rules you should try and follow when working on a project.

A basis to remember is that you should have a commit for every modification you make. Do not batch all your files using `git add .`, do not put a bugfix and a feature in the same commit.

>__3 rules to remember:__  
>1 - Do not commit half-baked work  
>2 - Commit often
>3 - Commit related files together


#### Commit content

The content of your commit should only include files linked to a specific feature, bugfix or edit. The more segmented your commits are, the easier it will be to track down, correct or revert a problem.

That does not mean that you should create one commit per file edited but on the contrary, that you should think in terms of functionnality block (if you edited 3 files to fix a bug you should have all 3 of them in the same commit).

For example 1 commit should contains one and only one of the following:
- a bugfix
- a documentation page or paragraph or an edit on the documentation
- a micro-feature
- a new class, a new function
- an optimization of the code
- etc

To choose which file you want to add to your commit, simply use `git status` to see the files modified since your last commit and then `git add ./folder/my-file` to add them individually.

Note that you can also choose to commit only part of a file using:

```shell
git add -p <filename>
```

This functionnality is a bit advanced but pretty useful to make clean commits. You can read more [here](https://nuclearsquid.com/writings/git-add/) or [here](https://git-scm.com/docs/git-add).


### Pushing code

Once you're done working on a functionnality, you can publish your code by pushing it on the distant repository.

First make sure that your current code is up to date:

```shell
git pull --rebase origin/feat/my-feature
```

Once that's done and you've fixed the possible conflicts, push your code:

```shell
git push origin feat/my-feature
```

>Note that it is not mandatory to specify the branch's name but you should always do so to avoid any mistake if you forgot to checkout


### Pull request

Note that only the administrator of the repository should (and should be able) to merge `dev` branch into `main`.
However anyone should be able to merge someone else's feature branch in `dev` after a meticular review of its content.

Once you are done on your branch create a new pull request. Most of the time it is way better to directly use the repository interface to create your pull request as you have a better view of the different tickets, comments and conflicts in one glance.

Though if you are a masochist you can use the following command syntax:

```shell
# first make sure that you are on the right branch
git checkout my-branch

# list all commits and differences between your branch and dev
git request-pull dev ./

# create your pull request
git request-pull [-p] <start> <url> [<end>]
```

In this case `<start>` is the id of the last commit available on the `dev` branch and `<end>` is the id of the last commit of your branch.

Without the `-p` parameter you will only see the result of your request but it will not be published.

You can also simply specify the branches:

```shell
git -p request-pull origin/main git@github.com:myusername/my-project.git origin/dev
```

Once your pull request is made, someone else should review it and merge it.

### H - Code Review

Code review is a simple but necessary task to produce a professional-grade project.

You should always have someone checking your branch before it being merged into `dev` to ensure its quality.

Here's how it work:  
![Code Review Workflow](https://oroinc.com/orocrm/wp-content/uploads/sites/8/2017/09/code-review-workflow-613x460.jpg)

#### 11 issues to look for

#### 1 - Readability a.k.a. Understandability
Readability in software means that the code is easy to understand. In this case, understanding code means being able to easily see the code’s inputs and outputs, what each line of code is doing, and how it fits into the bigger picture. When reading through the code, it should be relatively easy for you to discern the role of specific functions, methods, or classes.

#### 2 - Maintainability
One of the most common reasons that code eventually becomes painful to work with is because it isn’t written to be easily extendable and changeable.

Here are some warning signs that code may not be easy to maintain in the future:

- It’s very tightly coupled to another system.
- configuration is hard-coded
- It contributes to tech debt by increasing investment in a technology that the team wants to phase out
- It relies on old code that has been slated for refactoring

#### 3 - Security
Security vulnerabilities often enter codebases because developers write code without thinking about security. This might mean that they write insecure code that introduces vulnerabilities into the system, or use libraries and tools that are out-of-date or have known security issues.

Some things to check:
- is there credentials in the code or in configuration files?
- any key or certificate?
- when making request to a database using user inputs, are those sanitized correctly? 


#### 4 - Speed and Performance
Consider performance across two dimensions: performance for users and resource consumption. Performance for users reflects a focus on how quickly your code performs for the end user. Lengthy database queries, unoptimized assets, and multiple API requests can all work to make your code feel slow.

When possible, code should use lazy loading, as well as asynchronous and parallel processing. It should use caching as much as possible and shouldn't load anything that isn't used.

The other dimension of performance is resource consumption. This means not commissioning cloud servers that are more powerful than needed, not running intensive reports more frequently than needed, and otherwise, not putting the system under more load than it needs to be under as a result of code or infrastructure choices.

Do not take this need for speed too far, though. Doing so leads to optimizations that aren't noticeable to the user or aren’t worth the time investment. Be practical and focus on the 20% of optimizations that produce 80% of results.

#### 5 - Test Coverage and Test Quality

Code review is as important for tests as it is for the code that is tested. This is because a flawed test is more dangerous than having no test. Passing tests allows the developer to feel secure and willing to push new code to production, but what if one of the tests is passing for the wrong reason, or isn’t testing what it is supposed to test?

Tests should be readable, maintainable, performant, and adhere to established patterns. When it’s time to update or maintain existing code, its tests are likely to be the first thing that needs to change. 

Lastly, don’t stop at reviewing the tests that are there. Think through whether there are tests that are missing. Are there edge cases that haven’t been tested?

#### 6 - Documentation
Check whether the code you’re reviewing requires extra documentation to go along with it. 

Is the code commented?
If a new API is implemented, is the technical documentation updated?

#### 7 - Simplicty a.k.a. Reinventing the Wheel
Does the code use the right language features to get the job done? 

The code shouldn’t re-implement functions that already exist in the language or libraries that the project uses.

#### 8 - Reliability
Reliable code is written on the assumption that things will fail, that assets will sometimes not load, API requests will occasionally return 500 errors, and database records will be missing. When a certain level of failure is anticipated, it can be handled elegantly.

#### 9 - Scalability
Consider scalability by imagining what might happen to the code you’re reviewing if it were put under unexpected load.

If you are ingesting daily data, what would happens in case of a sudden peak of data?
What would happens if instead of 10 users account your applications suddenly get 1000s? Would your database handle it?

Think of it in terms of both performance as well as storage.


#### 10 - Reusability
Check that the code is written with likely future use-cases in mind. For example, if you're reviewing code for a marketplace that is rapidly expanding its product range, make sure that the code can easily be updated to support new kinds of products in the future.
By the same token, make sure that the code doesn’t take this too far by trying to account for use cases which are unlikely to eventuate.

Remember that code that is never used is immediately legacy code; check [Yagni principle](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it).

#### 11 - Homogeneity
Another consideration when adding new code to a codebase is whether it matches the code style established by the team.
Since you are using both a cleaner and a linter, it should by default not deviate from the codebase.



### Merging

To merge `feat/new_test_train_split` into `dev` just run the command:

```shell
# make sure you are on dev
git checkout dev

# start the merge
git merge feat/new_test_train_split

# if no conflict, push the modifications on the server
git push origin dev
```

#### When to use merge VS rebase

There are 2 ways to update a branch:
- `git merge` which merges a branch into another
- `git pull --rebase` which takes all commits on a distant branch and adds them to the current branch transparently. This means that the git history will always look flat as if no merge ever happened.

Given our current git flow you should stay away from `git pull --rebase`.
Note that `git merge` is not always better than `git pull --rebase`, both just have different use.

If you are interested by the topic I encourage you to check this [link](https://www.atlassian.com/git/tutorials/merging-vs-rebasing) to read more about it.

#### In case of conflict

Bad luck.

If this is your very first time encountering a merge conflict, you can set up a default merging tool by typing the following in your terminal:

```shell
git config --global merge.tool p4merge
git config --global mergetool.p4merge.cmd p4merge '$BASE $LOCAL $REMOTE $MERGED'
git config --global mergetool.p4merge.trustExitCode false
git config --global diff.tool p4merge
git config --global difftool.p4merge.cmd p4merge '$LOCAL $REMOTE'
```

When it's settled (or if you are sure enough of your `vim` skills), type:

```shell
git mergetool
```

to start to resolve the merging conflict.

### Tagging

Tagging is a way to set a release tag on a branch, allowing you to identify a specific release of your software (for example version `3.4.1`). This is especially important when working on a schedule or sprints with a client.

>You can use `git checkout 1.0.0` to travel to the state of a tag

When all your merge are done (especially if you are merging multiple features branch) in dev wait for your CI pipeline to validate the viability of your pre-release.


Once that's done, merge `dev` and `hotfix` into `main` and then tag main with the release version of your software.

For example, if it is version `1.0.0` type:

```shell
# make sure that you are on main
git checkout main
git tag -a 1.0.0
git push origin 1.0.0
```

Note that you can also add a message using the `-m` parameter.

#### Rules of tagging

All minor and major releases should be tagged.
What’s a minor release ?
- When the `hotfix` branch is merged in `main` it should be of the form `x.x.1`, then `x.x.2`, then `x.x.3` etc
  
  
What’s a major release ?
- When the `dev` branch is merged in `main` it should be of the form `1.0.0`, then `1.1.0`, then `1.2.0` etc

Moving from `1.0.0` to an upper version `2.0.0` should be made only for release with majors modifications removing most backward compatibilities, or with a new API, or radical UI evolutions etc.


### Q&A and tricks

I'll try to list some useful git commands that may help you speedup your developpement.

##### How to push/switch branch if I have uncommited change?

Commit all the files that you need to push and stash the rest:

```shell
git stash
```

That way you will record the current state of the working directory and the index and will go back to a clean working directory with your pending commits.

Use `git stash show` to view the files saved in your __last__ stash, `git stash list` to view all the differents stashed state.

To revert to a stash state use `git stash pop` (though if you want to keep the current work state you can use `git stash apply --index`).

Again, I advise your to read [this excellent article](https://www.atlassian.com/git/tutorials/saving-changes/git-stash) from Atlassian on the subject.

_____

##### List all commits

You can list all commits on the current branch with their titles by typing:

```shell
git log --oneline --graph
```

One of the many use is to check some stats like the number bug fix:

```shell
git log --oneline --graph | grep 'BF' | wc -l
```

____

##### Search code

In some case it can be interesting to look for specific word in your code.

To check on your current code type:

```shell
git grep -l 'password'
```

To search in all the code (including past commits and deleted files):

```shell
git log -S 'password'
```

____

##### Search for a specific file name

To search in all git history of a project for an existing or deleted files in its different version, use:

```shell
git log --all --full-history -- "**/thefile.*"
```

____

##### List commits by author

```shell
git log --author="<authorname>" --oneline --shortstat
```

____

##### Fix a typo in last commit (not pushed) message

```shell
git commit --amend -m "New commit message"
```

____

##### Add a file to the previous (not pushed) commit

```shell
git add <missing_file>
git commit --amend --no-edit
```
>_Do NOT amend a published commit_

____

##### Commit in the past/future

You promised your manager that your work would be done by the end of the day and its already tomorow? You want to show that you are a hard worker and still get 12 hours of sleep?
Just type:

```shell
git commit --date "2019-12-13T13:37:00"
```
_Note that this sadly does not change the date of your push_

____

##### Advanced tips

I won't be explaining things in this paragraph, only giving you links to pertinent articles.

- `git bissect`: pretty useful if you need to find a bug in your code but can't identify the source. [Link](https://www.metaltoad.com/blog/beginners-guide-git-bisect-process-elimination).

- `git reflog`: this command allows you to list all the commands that were executed and then recover particular commits or detached state. [Link](https://www.atlassian.com/git/tutorials/rewriting-history/git-reflog).

- `git cherry-pick`: this commands gives you the ability to take commits disctinctively and add them to your current HEAD state. [Link](https://www.atlassian.com/git/tutorials/cherry-pick).

- `git clean`: remove untracked files. [Link](https://www.atlassian.com/git/tutorials/undoing-changes/git-clean).

- `git prune`: remove unreachable objects. [Link](https://www.atlassian.com/git/tutorials/git-prune)


## Securité et Configuration :construction_worker:
