#!/bin/bash

files=(
    "./.emacs.d/elisp"
    "./.emacs"
    "./.zshrc"
    "./.tmux.conf"
)

echo "Copy all files to home directory? y/N"
read proceed

if [[ $proceed = "Y" ]]  || [[ $proceed = "y" ]]
then
    for file in "${files[@]}"
    do
        path="${HOME}/${file}"
        echo "Copying $file to ${path}"
        cp $file $path
    done
fi
