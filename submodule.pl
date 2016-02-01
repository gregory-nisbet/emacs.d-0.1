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
    ["https://github.com/ejmr/php-mode", "php-mode"]
);

chdir "$ENV{HOME}/.emacs.d/modules";

foreach my $item (@submodules) {
    my ($url, $dir) = @$item;
    `git submodule add $url $dir`;
}
