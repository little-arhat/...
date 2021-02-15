Prior to the installation make sure you have committed the alias to your .bashrc or .zsh:

```
alias config='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

And that your source repository ignores the folder where you'll clone it, so that you don't create weird recursion problems:

```
echo ".dotfiles" >> .gitignore
```

Now clone your dotfiles into a bare repository in a "dot" folder of your $HOME:

```
git clone --bare <git-repo-url> $HOME/.dotfiles
```

Define the alias in the current shell scope:

```
alias config='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
````

Checkout the actual content from the bare repository to your $HOME:

```
config checkout
```

The step above might fail with a message like:

```
error: The following untracked working tree files would be overwritten by checkout:
    .bashrc
    .gitignore
Please move or remove them before you can switch branches.
Aborting
```

Configure your repo:

```
config config --local status.showUntrackedFiles no
```


Remove or backup files and repeat.
https://www.atlassian.com/git/tutorials/dotfiles
