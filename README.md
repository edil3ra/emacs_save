# INSTALL
copy .emacs in the apropriate folder
run install-package use-package

# CONFIG
    apt-get install silversearcher-ag
    
## CONFIG BY LANGUAGE
### Javascript
    npm install -g eslint tern import-js js-beautify
    copy .tern-project .jsbeautifyrc .eslintrc in ~
    
### Typescript
    npm install typescript tslint -g 
    copy tslint.json in ~
    
### Python
    pip3 install jedi rope flake8 importmagic yapf
    copy ./config/flake8
    
### Php
    sudo apt-get install php-cli
    sudo apt-get install cscope
    copy .ac-php-conf.json in ~
