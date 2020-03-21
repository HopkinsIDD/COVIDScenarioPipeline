# Working with submodules

Some key concepts:

* At any given time your main repository needs to point to a *specific commit* in your submodule. On your computer, the files shown in the submodule folder will match the contents at that specific commit.

* When you update your submodules, by default your main repository will shift to point to the most recent commit on the *master branch* of the submodule. Main repositories can be configured to point to the most recent commit on another branch, if desired. Check the `.gitmodules` file to see if a branch is specified. If no branch is specified, updating your submodule will change the submodule contents to the most recent commit on the master branch.

* Pointing to a specific commit is not the same as *tracking a branch*. In order to commit and push changes in your submodule, you will need to ensure that your main repository is tracking a specific branch of the submodule.

* Confusingly, updating your submodule will change your main repository to not track any branch of your submodule. If you are not tracking, you can lose your data if you make local changes to the submodule and then update your repository again (which instantly moves the content to the newest commit, disregarding anything local, because it wasn't tracking).

* Seeing files in your submodule directory is **not** the same as tracking them. At all times we need to make sure (A) our submodule points to the most recent commit on the desired branch, and (B) we are tracking that same branch. Luckily, these are both easy tasks to accomplish -- we just need to remember to do them each time!


## Quick links

* [Cloning a repository with submodules](#cloning-the-repository-for-the-first-time)
* [Pulling changes](#pulling-changes-to-git-repositories-with-submodules)
* [Making changes to submodules](#making-changes-to-submodules-within-the-main-repository)
* [Pushing changes](#pushing-changes-to-git-repositories-with-submodules)
* [Switching branches](#switching-branches-when-working-with-submodules)

## Cloning a repository with submodules for the first time

To properly clone a git repository with submodules, run the following command for the main repository:

```
git clone --recurse-submodules REPOSITORY-URL
```

If you have already cloned the repository with the standard `git clone REPOSITORY-URL`, you will see the directories that contain the submodules, but they will be empty. In that case, you need to run the following two commands afterwards to fetch all the data from these submodules. **Don't run these commands if you properly cloned your repository with the --recurse-submodules flag above.**

```
git submodule init
git submodule update
```

Afer cloning your repository, run the following command to ensure that you are pointing to the most recent version of each submodule *in the specified branch for this repository*. You can check the branch that this repository points to by looking inside `.gitmodules`. If no branch is specified, this repository defaults to master.

```
git submodule update --remote --recursive
```

(This will update all of the submodules in your main repository. If there are many, consider adding `SUBMODULE-NAME` to the end of the command to update just one at a time.)

Since we just updated which commit in the submodule the main repository is pointing to, we need to commit these changes:

```
git add SUBMODULE-NAME
git commit -m 'update submodule version to most recent commit'
```

We also need to ensure we are tracking the desired branch so that any local changes don't get lost. This step isn't strictly necessary if you do not plan to update the submodule, but it's good practice to get in the habit of. The command below will ensure each submodule tracks the branch specified in the `.gitmodules` file.

```
git submodule foreach -q --recursive 'git checkout $(git config -f $toplevel/.gitmodules submodule.$name.branch || echo master)'
```

## Pulling changes to git repositories with submodules

Each time you want to pull new changes, including new changes to the submodules, do the following. Remember, the basic steps to updating submodules are:

1. Pull
2. Commit
3. Track

```
git pull
git submodule update --remote --recursive

git add SUBMODULE NAME
git commit -m 'git commit -m 'update submodule version to most recent commit'

git submodule foreach -q --recursive 'git checkout $(git config -f $toplevel/.gitmodules submodule.$name.branch || echo master)'
```

Note: a previous version of this document suggested using `git pull --recurse-submodules` instead of `git pull` followed by `git submodule update --remote --recursive`. This will still work as expected if your main repository points to the master branch of the submodule. However, when this is not the case, `git pull --recurse-submodules` will not update the pointer to the correct commit. Therefore, this has been updated to the current version, which will work regardless of the designated submodule branch.

## Making changes to submodules within the main repository

Before making any changes within the submodule directory, you need to check out a specific branch of that submodule. This is very important because `git submodule update` by default leaves the sub-repository in what's called a "detached HEAD" state. This means there is no local working branch tracking changes, and you may lose local changes the next time you run `git submodule update`.

If you correctly followed the steps in the section above, this step should be redudant. But let's do it just to be sure. This also shows you how to track a different branch, should you ever want to work on the non-default branch of your submodule.

```
cd SUBMODULE-NAME
git checkout SUBMODULE-BRANCH-NAME
```

Now you can make local changes and commit them as usual.

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

To avoid potential issues, make sure to commit and push all changes on a branch before switching. After switching, make sure to update this branch to point to the most recent commit on the submodule branch specified in this branch of the main repository, commit this change, and set up tracking of the submodule directory.