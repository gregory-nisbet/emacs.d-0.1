#!/usr/bin/env perl
use strict;
use warnings FATAL => 'all';
use autodie;

my @submodules = (
    ["https://github.com/cofi/evil-leader", "evil-leader"],
    ["https://github.com/ivucica/evil-mode", "evil-mode"],
    ["https://github.com/emacsmirror/nlinum", "nlinum"],
    ["https://github.com/emacsmirror/undo-tree", "undo-tree"],
    ["https://github.com/ocaml/tuareg", "tuareg"],
    # directory names shall never have an extension other than perhaps
    # .d
    ["https://github.com/dominikh/go-mode.el", "go-mode"],
    ["https://github.com/ejmr/php-mode", "php-mode"],
    ["https://github.com/hvesalai/scala-mode2", "scala-mode2"],
    ["https://github.com/haskell/haskell-mode", "haskell-mode"],

    # magit has a whole bunch of dependencies, so let's deal with those now
    ["https://github.com/magit/magit", "magit"],
    ["https://github.com/magit/git-modes", "git-modes"],
    ["https://github.com/magnars/dash.el", "dash"],
    ["https://github.com/magit/with-editor", "with-editor"],

    ["https://github.com/clojure-emacs/clojure-mode", "clojure-mode"],
);

chdir "$ENV{HOME}/.emacs.d/modules";

foreach my $item (@submodules) {
    my ($url, $dir) = @$item;
    `git submodule add $url $dir`;
}

# update recursive
system('git submodule update --init --recursive');

foreach my $item (@submodules) {
    my ($url, $dir) = @$item;
    my $cpid = fork;
    until (defined $cpid) {
        $cpid = fork;
        sleep 1;
    }
    if ($cpid == 0) {
        chdir $dir;
        if (-f 'Makefile') {
            system('make clean');
            system('make');
        }
        exit;
    }
}
