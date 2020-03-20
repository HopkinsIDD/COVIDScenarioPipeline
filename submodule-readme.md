# Working with submodules

## Quick links

* [Cloning the repository](#cloning-the-repository-for-the-first-time)
* [Pulling changes](#pulling-changes-to-git-repositories-with-submodules)
* [Making changes to submodules](#making-changes-to-submodules-within-the-main-repository)
* [Pushing changes](#pushing-changes-to-git-repositories-with-submodules)
* [Switching branches](#switching-branches-when-working-with-submodules)

## Cloning the repository for the first time

```
git clone --recurse-submodules REPOSITORY-URL
```

If you have already cloned the repository with the standard `git clone REPOSITORY-URL`, you will see the directories that contain the submodules, but they will be empty. You need to run the following two commands to fetch all the data from these submodules:

```
git submodule init
git submodule update
```

## Pulling changes to git repositories with submodules

If you have not made local changes to submodules, you can do the following to pull all changes and update all submodules to the latest version:

```
git pull --recurse-submodules

```

If you have made local changes to a submodule and you want to pull and merge in changes from the server:

```
git submodule update --remote --merge SUBMODULE-NAME
```

After either of these commands, remember to commit the submodule update so that your code now references an updated submodule commit:

```
git commit -m 'updated submodule version to most recent commit'
```

## Making changes to submodules within the main repository

Before making any changes within the submodule directory, you need to check out a specific branch of that submodule. This is very important because `git submodule update` by default leaves the sub-repository in what's called a "detached HEAD" state. This means there is no local working branch tracking changes, and you may lose local changes the next time you run `git submodule update`.

```
cd SUBMODULE-NAME
git checkout SUBMODULE-BRANCH-NAME
```

If you find yourself doing this frequently, you can run the following command after updating the submodules (either through `git pull --recurse-submodules` or `git submodule update`) to always checkout the designated branch for each submodule:

```
git submodule foreach -q --recursive 'git checkout $(git config -f $toplevel/.gitmodules submodule.$name.branch || echo master)'
```

If you are not very familiar with submodules, you can reduce possible confusion by only updating the submodule from within the submodule repository itself, and not from within the main project repository (i.e., don't make changes to COVIDScenarioPipeline from within another project folder -- navigate to a separate directory containing only the COVIDScenarioPipeline repository.)

## Pushing changes to git repositories with submodules

Use the following command to push changes, including all committed submodule changes:

```
git push --recurse-submodules=on-demand
```

## Switching branches when working with submodules

When working with submodules, switch branches in the main repository like this to avoid any issues:

```
git checkout --recurse-submdoules BRANCH-NAME
```