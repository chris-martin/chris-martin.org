--------------------------------------------------------------------------------
title:    What goes in $PATH?
date:     2018 Apr 24
slug:     what-goes-in-path
css:      path.css
abstract: A quick overview of the purpose of the `PATH` variable.
--------------------------------------------------------------------------------

The `PATH` environment variable lists all the places where you have programs installed. It is customizable because there are a lot of package managers, and they all install things into different places, so your `PATH` needs to be tailored to the package managers that *you* use.

Here are some of the package managers I use and the places where they install programs:

<table class="package-managers">
  <tr>
    <td><a href="https://nixos.org/nix/manual/">Nix</a></td>
    <td><code>$HOME/.nix-profile/bin</code></td>
  </tr>
  <tr>
    <td><a href="https://nixos.org/nixos/manual/">NixOS</a></td>
    <td><code>/run/current-system/sw/bin</code></td>
    </tr>
  <tr>
    <td><a href="https://www.haskell.org/cabal/">Cabal</a></td>
    <td><code>$HOME/.cabal/bin</code></td>
  </tr>
  <tr>
    <td><a href="https://www.haskellstack.org/">Stack</a></td>
    <td><code>$HOME/.local/bin</code></td>
  </tr>
  <tr>
    <td><a href="https://www.npmjs.com/">npm</a></td>
    <td><code>./node_modules/.bin</code></td>
  </tr>
</table>

For our purposes here, the words “program,” “executable,” and “binary” are used synonymously. As you can see above, directories that are intended to be used with `PATH` tend to be named “bin,” signifying that they are full of binaries.

`PATH` may be referred to as “the binary search path,” “the search path,” “the bin path,” or simply “the path.”

## An example

Suppose you use Stack to install pandoc, but you get “command not found” when you try to run it.

```bash
$ stack install pandoc

$ pandoc --version
pandoc: command not found
```

Why didn't it work? Probably because pandoc was installed to a place that isn't on your search path. To fix this sort of problem, you need to:

1. Consult the package manager's documentation to find out where it installs things.
2. Add that location to the search path (below we'll see how to do that).

### Debugging by bypassing the search path

We installed using Stack, and we know that Stack puts programs into `~/.local/bin`. First let's verify that pandoc is indeed there by running it directly, without relying on the search path:

```bash
$ ~/.local/bin/pandoc --version
pandoc 1.19.2.1
```

That worked, so next let's fix the search path.

### Adding a directory to the path

The way you do this will be different depending on what shell you're using. Here I will assume your shell is Bash.

```bash
$ export PATH="$HOME/.local/bin:$PATH"
```

Now we should be able to run pandoc normally.

```bash
$ pandoc --version
pandoc 1.19.2.1
```

### Making the change permanent

So we've fixed the path, but what we've done so far is only temporary. When you restart your terminal, it will be gone.

Bash has a mechanism for doing things like this: If you have a file named `~/.bashrc`, Bash will automatically run all the commands in that file every time you start a terminal. So open `~/.bashrc` (if it doesn't exist yet, just create a blank file) and add the following anywhere in it:

```bash
# This adds Stack's install directory to the search path
# so we can run programs installed with 'stack install'.
export PATH="$HOME/.local/bin:$PATH"
```

Don't neglect to include the comment, or else as stuff accumulates in your `~/.bashrc` file over months and years, you'll end up with no recollection of why you put it there.

Note that changes to `~/.bashrc` are *not* automatically applied to terminals that are already open. You will need to restart each open terminal, or run

```bash
$ source ~/.bashrc
```

in each terminal to re-run the initialization script.
